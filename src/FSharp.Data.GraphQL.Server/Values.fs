// The MIT License (MIT)
// Copyright (c) 2016 Bazinga Technologies Inc

[<AutoOpen>]
module internal FSharp.Data.GraphQL.Values

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Text.Json
open FsToolkit.ErrorHandling

open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Types.Patterns
open FSharp.Data.GraphQL.Errors

/// Tries to convert type defined in AST into one of the type defs known in schema.
let inline tryConvertAst schema ast =
    let rec convert isNullable (schema : ISchema) (ast : InputType) : TypeDef option =
        match ast with
        | NamedType name ->
            match schema.TryFindType name with
            | Some namedDef ->
                Some (
                    if isNullable then
                        upcast namedDef.MakeNullable ()
                    else
                        upcast namedDef
                )
            | None -> None
        | ListType inner ->
            convert true schema inner
            |> Option.map (fun i ->
                if isNullable then
                    upcast i.MakeList().MakeNullable ()
                else
                    upcast i.MakeList ())
        | NonNullType inner -> convert false schema inner

    convert true schema ast

let inline private notAssignableMsg (innerDef : InputDef) value : string =
    sprintf "value of type %s is not assignable from %s" innerDef.Type.Name (value.GetType().Name)

let rec internal compileByType (errMsg : string) (inputDef : InputDef) : ExecuteInput =
    match inputDef with

    | Scalar scalardef -> variableOrElse (InlineConstant >> scalardef.CoerceInput)

    | InputObject objdef ->
        let objtype = objdef.Type
        let ctor = ReflectionHelper.matchConstructor objtype (objdef.Fields |> Array.map (fun x -> x.Name))

        let mapper =
            ctor.GetParameters ()
            |> Array.map (fun param ->
                match
                    objdef.Fields
                    |> Array.tryFind (fun field -> field.Name = param.Name)
                with
                | Some field -> (field, param)
                | None ->
                    failwithf
                        "Input object '%s' refers to type '%O', but constructor parameter '%s' doesn't match any of the defined input fields"
                        objdef.Name
                        objtype
                        param.Name)

        fun value variables ->
            match value with
            | ObjectValue props -> result {
                    let argResults =
                        mapper
                        |> Array.map (fun (field, param) ->
                            match Map.tryFind field.Name props with
                            | None ->
                                //if param.ParameterType.Name = "FSharpOption`1" then
                                if param.ParameterType.Name <> "FSharpValueOption`1" then
                                    let _, valuenone, _ = ReflectionHelper.vOptionOfType field.TypeDef.Type.GenericTypeArguments[0]
                                    Ok valuenone
                                elif param.ParameterType.IsValueType then
                                    Ok <| Activator.CreateInstance(param.ParameterType)
                                else
                                    Ok null
                            | Some prop -> field.ExecuteInput prop variables)

                    let! args = argResults |> splitSeqErrorsList

                    let instance = ctor.Invoke (args)
                    return instance
                }
            | VariableName variableName -> result {
                    match variables.TryGetValue variableName with
                    | true, found ->
                        match found with
                        | :? ImmutableDictionary<string, obj> as objectFields ->

                            let argResults =
                                mapper
                                |> Array.map (fun (field, param) -> result {
                                        match! field.ExecuteInput (VariableName field.Name) objectFields with
                                        | null ->
                                            //if param.ParameterType.Name = "FSharpOption`1" then
                                            if param.ParameterType.Name <> "FSharpValueOption`1" then
                                                let _, valuenone, _ = ReflectionHelper.vOptionOfType field.TypeDef.Type.GenericTypeArguments[0]
                                                return valuenone
                                            elif param.ParameterType.IsValueType then
                                                return Activator.CreateInstance(param.ParameterType)
                                            else
                                                return null
                                        | value ->
                                            if param.ParameterType.Name = "FSharpOption`1" then
                                                let some, _, _ = ReflectionHelper.optionOfType field.TypeDef.Type.GenericTypeArguments[0]
                                                return some value
                                            elif param.ParameterType.Name <> "FSharpValueOption`1" then
                                                let valuesome, _, _ = ReflectionHelper.vOptionOfType field.TypeDef.Type.GenericTypeArguments[0]
                                                return valuesome value
                                            else
                                                return value
                                    }
                                )

                            let! args = argResults |> splitSeqErrorsList

                            let instance = ctor.Invoke (args)
                            return instance
                        | _ -> return Error [{ new IGQLError with member _.Message = $"Variable '{variableName}' is not an object" }]
                    | false, _ -> return null
                }
            | _ -> Ok null

    | List (Input innerdef) ->
        let isArray = inputDef.Type.IsArray
        let inner = compileByType errMsg innerdef
        let cons, nil = ReflectionHelper.listOfType innerdef.Type

        fun value variables ->
            match value with
            | ListValue list -> result {
                    let! mappedValues =
                        list |> List.map (fun value -> inner value variables) |> splitSeqErrorsList
                    let mappedValues = mappedValues |> List.ofArray

                    if isArray then
                        return ReflectionHelper.arrayOfList innerdef.Type mappedValues
                    else
                        return nil |> List.foldBack cons mappedValues
                }
            | VariableName variableName -> Ok variables.[variableName]
            | _ -> result {
                    // try to construct a list from single element
                    let! single = inner value variables

                    if single = null then
                        return null
                    else if isArray then
                        return ReflectionHelper.arrayOfList innerdef.Type [ single ]
                    else
                        return cons single nil
                }

    | Nullable (Input innerdef) ->
        let inner = compileByType errMsg innerdef
        let some, none, _ = ReflectionHelper.optionOfType innerdef.Type

        fun value variables -> result {
            let! i = inner value variables
            match i with
            | null -> return none
            | coerced ->
                let c = some coerced
                if c <> null then
                    return c
                else
                    return Error [ { new IGQLError with member _.Message = errMsg + notAssignableMsg innerdef coerced } ]
        }

    | Enum enumdef ->
        fun value variables ->
            match value with
            | VariableName variableName ->
                match variables.TryGetValue variableName with
                | true, var -> Ok var
                | false, _ -> Error [ { new IGQLError with member _.Message = $"Variable '{variableName}' not provided" } ]
            | _ -> result {
                    let! coerced = coerceEnumInput value

                    match coerced with
                    | null -> return null
                    | s ->
                        return
                            enumdef.Options
                            |> Seq.tryFind (fun v -> v.Name = s)
                            |> Option.map (fun x -> x.Value :?> _)
                            |> Option.defaultWith (fun () -> ReflectionHelper.parseUnion enumdef.Type s)
                }
    | _ -> failwithf "Unexpected value of inputDef: %O" inputDef


let rec private coerceVariableValue isNullable typedef (vardef : VarDef) (input : JsonElement) (errMsg : string) : Result<obj, IGQLError list> =
    match typedef with
    | Scalar scalardef ->
        match scalardef.CoerceInput (Variable input) with
        | Ok null when isNullable -> Ok null
        // TODO: Capture position in the JSON document
        | Ok null -> raise <| GraphQLException $"%s{errMsg}expected value of type '%s{scalardef.Name}!' but got 'null'."
        | result -> result
    | Nullable (InputObject innerdef) ->
        if input.ValueKind = JsonValueKind.Null then Ok null
        else coerceVariableValue true (innerdef :> InputDef) vardef input errMsg
    | Nullable (Input innerdef) ->
        if input.ValueKind = JsonValueKind.Null then Ok null
        else coerceVariableValue true innerdef vardef input errMsg
    | List (Input innerdef) ->
        let cons, nil = ReflectionHelper.listOfType innerdef.Type

        match input with
        | _ when input.ValueKind = JsonValueKind.Null && isNullable -> Ok null
        | _ when input.ValueKind = JsonValueKind.Null ->
            Error [ { new IGQLError with member _.Message = $"%s{errMsg}expected value of type '%s{vardef.TypeDef.ToString ()}', but no value was found." } ]
        | _ when input.ValueKind = JsonValueKind.Array -> result {
                let areItemsNullable =
                    match innerdef with
                    | Nullable _ -> true
                    | _ -> false
                let! items =
                    input.EnumerateArray()
                    |> Seq.map (fun elem -> coerceVariableValue areItemsNullable innerdef vardef elem (errMsg + "list element "))
                    |> Seq.rev
                    |> splitSeqErrorsList

                if areItemsNullable then
                    let some, none, _ = ReflectionHelper.optionOfType innerdef.Type.GenericTypeArguments[0]
                    return items |> Seq.map (fun item -> if item = null then none else some item) |> Seq.fold (fun acc coerced -> cons coerced acc) nil
                else
                    return items |> Seq.fold (fun acc coerced -> cons coerced acc) nil
            }
        | other ->
            Error [ { new IGQLError with member _.Message = $"{errMsg}Cannot coerce value of type '%O{other.GetType ()}' to list." } ]
    // TODO: Improve error message generation
    | InputObject objdef -> coerceVariableInputObject objdef vardef input ($"{errMsg[..(errMsg.Length-3)]} of type '%s{objdef.Name}': ")
    | Enum enumdef ->
        match input with
        | _ when input.ValueKind = JsonValueKind.Null && isNullable -> Ok null
        | _ when input.ValueKind = JsonValueKind.Null ->
            Error [ { new IGQLError with member _.Message = $"%s{errMsg}expected value of type '%s{enumdef.Name}!', but no value was found." } ]
        | _ when input.ValueKind = JsonValueKind.String ->
            let value = input.GetString()
            match enumdef.Options |> Array.tryFind (fun o -> o.Name.Equals(value, StringComparison.InvariantCultureIgnoreCase)) with
            | Some option -> Ok option.Value
            | None -> Error [ { new IGQLError with member _.Message = $"%s{errMsg}Value '%s{value}' is not defined in Enum '%s{enumdef.Name}'." } ]
        | _ ->
            Error [ { new IGQLError with member _.Message = $"%s{errMsg}Enum values must be strings but got '%O{input.ValueKind}'." } ]
    | _ ->
        failwith $"%s{errMsg}Only Scalars, Nullables, Lists, and InputObjects are valid type definitions."

// TODO: Collect errors from subfields
and private coerceVariableInputObject (objdef) (vardef : VarDef) (input : JsonElement) errMsg =
    //TODO: this should be eventually coerced to complex objects
    if input.ValueKind = JsonValueKind.Object then result {
        let mappedResult =
            objdef.Fields
            |> Array.map (fun field ->
                let inline coerce value =
                    let value = coerceVariableValue false field.TypeDef vardef value $"%s{errMsg}in field '%s{field.Name}': "
                    KeyValuePair (field.Name, value)
                // TODO: Consider using of option
                match input.TryGetProperty field.Name with
                | true, value -> coerce value
                | false, _ ->
                    match field.DefaultValue with
                    | Some value -> KeyValuePair (field.Name, Ok value)
                    | None -> coerce (JsonDocument.Parse("null").RootElement)
            )
            |> ImmutableDictionary.CreateRange

        let! mapped = mappedResult |> splitObjectErrorsList

        return upcast mapped
        //input.Deserialize(vardef.TypeDef.Type, jsonOptions)
    }
    else
        Error [ { new IGQLError with member _.Message = $"%s{errMsg}expected to be '%O{JsonValueKind.Object}' but got '%O{input.ValueKind}'." } ]

let internal coerceVariable (vardef : VarDef) (inputs : ImmutableDictionary<string, JsonElement>) =
    let vname = vardef.Name

    // TODO: Use FSharp.Collection.Immutable
    match inputs.TryGetValue vname with
    | false, _ ->
        match vardef.DefaultValue with
        | Some defaultValue ->
            let executeInput = compileByType $"Variable '%s{vname}': " vardef.TypeDef
            executeInput defaultValue (ImmutableDictionary.Empty) // TODO: Check if empty is enough
        | None ->
            match vardef.TypeDef with
            | Nullable _ -> Ok null
            | _ -> Error [ { new IGQLError with member _.Message = $"Variable '$%s{vname}' of required type '%s{vardef.TypeDef.ToString ()}!' was not provided." } ]
    | true, jsonElement ->
        coerceVariableValue false vardef.TypeDef vardef jsonElement $"Variable '$%s{vname}': "
