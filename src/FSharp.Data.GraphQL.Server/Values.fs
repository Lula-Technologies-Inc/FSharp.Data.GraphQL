// The MIT License (MIT)
// Copyright (c) 2016 Bazinga Technologies Inc

[<AutoOpen>]
module internal FSharp.Data.GraphQL.Values

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Text.Json
open FSharp.Collections.Immutable
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Types.Patterns

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
                | Some field -> field
                | None ->
                    failwithf
                        "Input object '%s' refers to type '%O', but constructor parameter '%s' doesn't match any of the defined input fields"
                        objdef.Name
                        objtype
                        param.Name)

        fun value variables ->
            match value with
            | ObjectValue props ->
                let argResults =
                    mapper
                    |> Seq.map (fun field ->
                        match Map.tryFind field.Name props with
                        | None -> Ok null
                        | Some prop -> field.ExecuteInput prop variables)
                    |> Seq.toFlatList

                match argResults with
                | ArrayErrors errs -> Error errs
                | ArrayValues args ->
                    let instance = ctor.Invoke (args)
                    Ok instance
            | VariableName variableName ->
                match variables.TryGetValue variableName with
                | true, found ->
                    let variables = found :?> ImmutableDictionary<string, obj>
                    let args =
                        mapper
                        |> Array.map (fun field ->
                            match variables.TryGetValue field.Name with
                            | true, value -> value
                            | false, _ -> null)

                    let instance = ctor.Invoke (args)
                    Ok instance
                | false, _ -> Ok null
            | _ -> Ok null
    | List (Input innerdef) ->
        let isArray = inputDef.Type.IsArray
        let inner = compileByType errMsg innerdef
        let cons, nil = ReflectionHelper.listOfType innerdef.Type

        fun value variables ->
            match value with
            | ListValue list ->
                let results = list |> List.map (fun value -> inner value variables) |> FlatList.ofSeq
                match results with
                | ListErrors errs -> Error errs
                | ListValues mappedValues ->

                if isArray then
                    Ok <| ReflectionHelper.arrayOfList innerdef.Type mappedValues
                else
                    nil |> List.foldBack cons mappedValues |> Ok
            | VariableName variableName -> variables.[variableName] |> Ok
            | _ ->
                // try to construct a list from single element
                let result = inner value variables
                match result with
                | Error errors -> Error errors
                | Ok single ->
                    if single = null then Ok null
                    else if isArray then
                        Ok <| ReflectionHelper.arrayOfList innerdef.Type [ single ]
                    else
                        Ok <| cons single nil
    | Nullable (Input innerdef) ->
        fun variables value -> Ok value
    | Enum enumdef ->
        fun value variables ->
            match value with
            | VariableName variableName ->
                match variables.TryGetValue variableName with
                | true, var -> Ok var
                | false, _ -> Error [{ new IGQLError with member _.Message = $"Variable '%s{variableName}' not supplied.{Environment.NewLine}Variables:{Environment.NewLine}%A{variables}" }]
            | _ ->
                let coerced = coerceEnumInput value

                match coerced with
                | Ok null -> Ok null
                | Ok s ->
                    enumdef.Options
                    |> Seq.tryFind (fun v -> v.Name = s)
                    |> Option.map (fun x -> x.Value :?> _)
                    |> Option.defaultWith (fun () -> ReflectionHelper.parseUnion enumdef.Type s)
                    |> Ok
                | Error errs -> Error errs
    | _ -> failwithf "Unexpected value of inputDef: %O" inputDef

let rec private coerceVariableValue isNullable typedef (vardef : VarDef) (input : JsonElement) (errMsg : string) : GQLResult =
    match typedef with
    | Scalar scalardef ->
        if input.ValueKind = JsonValueKind.Null && isNullable then Ok null
        else scalardef.CoerceInput (Variable input)
    | Nullable (InputObject innerdef) ->
        coerceVariableValue true (innerdef :> InputDef) vardef input errMsg
    | Nullable (Input innerdef) ->
        if input.ValueKind = JsonValueKind.Null && isNullable then Ok null
        else coerceVariableValue true innerdef vardef input errMsg
    | List (Input innerdef) ->
        match input with
        | _ when input.ValueKind = JsonValueKind.Null && isNullable -> Ok null
        | _ when input.ValueKind = JsonValueKind.Null ->
            Error [{ new IGQLError with member _.Message = $"%s{errMsg}expected value of type '%s{vardef.TypeDef.ToString ()}', but no value was found." }]
        | _ when input.ValueKind = JsonValueKind.Array ->
            let cons, nil = ReflectionHelper.listOfType innerdef.Type
            let results =
                input.EnumerateArray()
                |> Seq.map (fun elem -> coerceVariableValue false innerdef vardef elem (errMsg + "list element "))
                |> Seq.toFlatList

            match results with
            | ArrayErrors errs -> Error errs
            | ArrayValues values ->
                let mapped = values |> Seq.fold (fun acc coerced -> cons coerced acc) nil
                Ok mapped
        | other ->
            Error [{ new IGQLError with member _.Message = $"{errMsg}Cannot coerce value of type '%O{other.GetType ()}' to list." }]
    // TODO: Improve error message generation
    | InputObject objdef -> coerceInputObjectVariable objdef vardef input ($"{errMsg[..(errMsg.Length-3)]} of type '%s{objdef.Name}': ")
    | Enum enumdef ->
        match input with
        | _ when input.ValueKind = JsonValueKind.Null && isNullable -> Ok null
        | _ when input.ValueKind = JsonValueKind.Null ->
            Error [{ new IGQLError with member _.Message = $"%s{errMsg}Expected Enum '%s{enumdef.Name}', but no value was found." }]
        | _ when input.ValueKind = JsonValueKind.String ->
            let value = input.GetString()
            match enumdef.Options |> Array.tryFind (fun o -> o.Name.Equals(value, StringComparison.InvariantCultureIgnoreCase)) with
            | Some option -> Ok option.Value
            | None ->
                Error [{ new IGQLError with member _.Message = $"%s{errMsg}Value '%s{value}' is not defined in Enum '%s{enumdef.Name}'." }]
        | _ ->
            Error [{ new IGQLError with member _.Message = $"%s{errMsg}Enum values must be strings but got '%O{input.ValueKind}'." }]
    | _ ->
        Error [{ new IGQLError with member _.Message = $"%s{errMsg}Only Scalars, Nullables, Lists, and InputObjects are valid type definitions." }]

// TODO: Collect errors from subfields
and private coerceInputObjectVariable (objdef) (vardef : VarDef) (input : JsonElement) errMsg =
    //TODO: this should be eventually coerced to complex object
    if input.ValueKind = JsonValueKind.Object then
        let results =
            objdef.Fields
            |> Seq.map (fun field ->
                // TODO: Consider using of option
                let value =
                    match input.TryGetProperty field.Name with
                    | true, valueFound -> valueFound
                    | false, _ -> JsonDocument.Parse("null").RootElement
                kvp field.Name (coerceVariableValue false field.TypeDef vardef value $"%s{errMsg}in field '%s{field.Name}': "))
            |> Seq.toHashMap

        match results with
        | ObjectErrors errs -> Error errs
        | ObjectValues mapped -> Ok (upcast mapped)
    else
        Error [{ new IGQLError with member _.Message = $"%s{errMsg}expected to be '%O{JsonValueKind.Object}' but got '%O{input.ValueKind}'." }]

let internal coerceVariable (vardef : VarDef) (inputs : ImmutableDictionary<string, JsonElement>) =
    let vname = vardef.Name

    // TODO: Use FSharp.Collection.Immutable
    match inputs.TryGetValue vname with
    | false, _ ->
        match vardef.DefaultValue with
        | Some defaultValue ->
            let errMsg = (sprintf "Variable '%s': " vname)
            let executeInput = compileByType errMsg vardef.TypeDef
            executeInput defaultValue (ImmutableDictionary.Empty) // TODO: Check if empty is enough
        | None ->
            match vardef.TypeDef with
            | Nullable _ -> Ok null
            | _ ->
                Error [{ new IGQLError with member _.Message = $"Variable '$%s{vname}' of required type '%s{vardef.TypeDef.ToString ()}' has no value provided." }]
    | true, jsonElement ->
        coerceVariableValue false vardef.TypeDef vardef jsonElement (sprintf "Variable '$%s': " vname)
