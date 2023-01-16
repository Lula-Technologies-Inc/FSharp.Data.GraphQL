// The MIT License (MIT)
// Copyright (c) 2016 Bazinga Technologies Inc
[<AutoOpen>]
module FSharp.Data.GraphQL.Types.SchemaDefinitions

open System
open System.Collections.Immutable
open System.Globalization
open System.Reflection
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Extensions
open FSharp.Data.GraphQL.Types
open FSharp.Quotations
open System.Text.Json


/// Tries to convert any value to int.
let private coerceIntValue (x : obj) : GQLResult<int> =
    match x with
    | null -> Error [{ new IGQLError with member _.Message = $"Cannot convert null to integer32" }]
    | other ->
        try Ok (System.Convert.ToInt32 other)
        with _ -> Error [{ new IGQLError with member _.Message = $"Cannot convert '%O{other}' to integer32" }]

/// Tries to convert any value to int64.
let private coerceLongValue (x : obj) : GQLResult<int64> =
    match x with
    | null -> Error [{ new IGQLError with member _.Message = $"Cannot convert null to integer64" }]
    | :? int as i -> Ok (int64 i)
    | :? int64 as l -> Ok (l)
    | :? double as d -> Ok (int64 d)
    | :? string as s ->
        match Int64.TryParse(s) with
        | true, i -> Ok i
        | false, _ -> Error [{ new IGQLError with member _.Message = $"Cannot parse '%s{s}' as integer64" }]
    | :? bool as b ->
        Ok (if b then 1L else 0L)
    | other ->
        try Ok (System.Convert.ToInt64 other)
        with _ -> Error [{ new IGQLError with member _.Message = $"Cannot convert '%O{other}' to integer64" }]

/// Tries to convert any value to double.
let private coerceFloatValue (x : obj) : GQLResult<double> =
    match x with
    | null -> Error [{ new IGQLError with member _.Message = $"Cannot convert null to float" }]
    | :? int as i -> Ok (double i)
    | :? int64 as l -> Ok (double l)
    | :? double as d -> Ok d
    | :? string as s ->
        match Double.TryParse(s) with
        | true, i -> Ok i
        | false, _ -> Error [{ new IGQLError with member _.Message = $"Cannot parse '%s{s}' as float" }]
    | :? bool as b ->
        Ok (if b then 1. else 0.)
    | other ->
        try Ok (System.Convert.ToDouble other)
        with _ -> Error [{ new IGQLError with member _.Message = $"Cannot convert '%O{other}' to float" }]

/// Tries to convert any value to bool.
let private coerceBoolValue (x : obj) : GQLResult<bool> =
    match x with
    | null -> Error [{ new IGQLError with member _.Message = $"Cannot convert null to boolean" }]
    | other ->
        try Ok (System.Convert.ToBoolean other)
        with _ -> Error [{ new IGQLError with member _.Message = $"Cannot convert '%O{other}' to boolean" }]

/// Tries to convert any value to URI.
let private coerceUriValue (x : obj) : GQLResult<Uri> =
    match x with
    | null -> Error [{ new IGQLError with member _.Message = $"Cannot convert null to URI" }]
    | :? Uri as u -> Ok u
    | :? string as s ->
        match Uri.TryCreate(s, UriKind.RelativeOrAbsolute) with
        | true, i -> Ok i
        | false, _ -> Error [{ new IGQLError with member _.Message = $"Cannot parse '%s{s}' as URI" }]
    | other ->
        Error [{ new IGQLError with member _.Message = $"Cannot convert '%O{other}' to URI" }]

/// Tries to convert any value to DateTimeOffset.
let private coerceDateTimeOffsetValue (x : obj) : GQLResult<DateTimeOffset> =
    match x with
    | null -> Error [{ new IGQLError with member _.Message = $"Cannot convert null to DateTimeOffset" }]
    | :? DateTimeOffset as d -> Ok d
    | :? DateTime as d -> Ok (DateTimeOffset d)
    | :? string as s ->
        match DateTimeOffset.TryParse(s) with
        | true, date -> Ok date
        | false, _ -> Error [{ new IGQLError with member _.Message = $"Cannot parse '%s{s}' as DateTimeOffset" }]
    | other ->
        Error [{ new IGQLError with member _.Message = $"Cannot convert '%O{other}' to DateTimeOffset" }]

/// Tries to convert any value to DateOnly.
let coerceDateOnlyValue (x : obj) : GQLResult<DateOnly> =
    match x with
    | null -> Error [{ new IGQLError with member _.Message = $"Cannot convert null to DateOnly" }]
    | :? DateOnly as d -> Ok d
    | :? DateTime as d -> Ok (DateOnly.FromDateTime d)
    | :? string as s ->
        match DateOnly.TryParse(s) with
        | true, date -> Ok date
        | false, _ -> Error [{ new IGQLError with member _.Message = $"Cannot parse '%s{s}' as DateOnly" }]
    | other -> Error [{ new IGQLError with member _.Message = $"Cannot convert '%O{other}' to DateOnly" }]

/// Tries to convert any value to Guid.
let private coerceGuidValue (x : obj) : GQLResult<Guid> =
    match x with
    | null -> Error [{ new IGQLError with member _.Message = $"Cannot convert null to GUID" }]
    | :? Guid as g -> Ok g
    | :? string as s ->
        match Guid.TryParse(s) with
        | true, g -> Ok g
        | false, _ -> Error [{ new IGQLError with member _.Message = $"Cannot parse '%s{s}' as GUID" }]
    | other ->
        Error [{ new IGQLError with member _.Message = $"Cannot convert '%O{other}' to GUID" }]

/// Check if provided obj value is an Option and extract its wrapped value as object if possible
let private (|Option|_|) (x : obj) =
    if x = null then None
    else
        let t = x.GetType().GetTypeInfo()
        if t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<option<_>> then
            t.GetDeclaredProperty("Value").GetValue(x) |> Some
        else None

/// Tries to convert any value to string.
let coerceStringValue (x : obj) : GQLResult<string> =
    match x with
    | null -> Error [{ new IGQLError with member _.Message = $"Cannot convert null to string" }]
    | :? string as s -> Ok s
    | :? bool as b -> Ok (if b then "true" else "false")
    | Option o -> Ok (o.ToString())
    | _ -> Ok (x.ToString())

/// Tries to convert any value to generic type parameter.
let private coerceIdValue (x : obj) : GQLResult<'t> =
    match x with
    | null -> Error [{ new IGQLError with member _.Message = $"Cannot convert null to {typeof<'t>.Name}" }]
    | :? string as s -> Ok (downcast Convert.ChangeType(s, typeof<'t>))
    | Option o -> Ok (downcast Convert.ChangeType(o, typeof<'t>))
    | _ -> Ok (downcast Convert.ChangeType(x, typeof<'t>))

/// Tries to resolve AST query input to int.
let coerceIntInput =
    function
    | Variable e when e.ValueKind = JsonValueKind.Number -> Ok (e.GetInt32())
    | InlineConstant (IntValue i) -> Ok (int i)
    | InlineConstant (FloatValue f) -> Ok (int f)
    | InlineConstant (StringValue s) ->
        match Int32.TryParse(s, NumberStyles.Float, CultureInfo.InvariantCulture) with
        | true, i -> Ok i
        | false, _ -> Error [{ new IGQLError with member _.Message = $"Cannot parse '%s{s}' as integer32" }]
    | InlineConstant (BooleanValue b) -> Ok (if b then 1 else 0)
    | other ->
        Error [{ new IGQLError with member _.Message = $"Cannot convert '%O{other}' to integer32" }]

/// Tries to resolve AST query input to int64.
let coerceLongInput =
    function
    | Variable e when e.ValueKind = JsonValueKind.Number -> Ok (e.GetInt64())
    | InlineConstant (IntValue i) -> Ok (int64 i)
    | InlineConstant (FloatValue f) -> Ok (int64 f)
    | InlineConstant (StringValue s) ->
        match Int64.TryParse(s, NumberStyles.Float, CultureInfo.InvariantCulture) with
        | true, i -> Ok i
        | false, _ -> Error [{ new IGQLError with member _.Message = $"Cannot parse '%s{s}' as integer64" }]
    | InlineConstant (BooleanValue b) -> Ok (if b then 1L else 0L)
    | other ->
        Error [{ new IGQLError with member _.Message = $"Cannot convert '%O{other}' to integer64" }]

/// Tries to resolve AST query input to double.
let coerceFloatInput =
    function
    | Variable e when e.ValueKind = JsonValueKind.Number -> Ok (e.GetDouble())
    | InlineConstant (IntValue i) -> Ok (double i)
    | InlineConstant (FloatValue f) -> Ok f
    | InlineConstant (StringValue s) ->
        match Double.TryParse(s, NumberStyles.Float, CultureInfo.InvariantCulture) with
        | true, i -> Ok i
        | false, _ -> Error [{ new IGQLError with member _.Message = $"Cannot parse '%s{s}' as float" }]
    | InlineConstant (BooleanValue b) -> Ok (if b then 1. else 0.)
    | other ->
        Error [{ new IGQLError with member _.Message = $"Cannot convert '%O{other}' to float" }]

/// Tries to resolve AST query input to string.
let coerceStringInput =
    function
    | Variable e ->
        match e.ValueKind with
        | JsonValueKind.String -> Ok (e.GetString())
        | JsonValueKind.True
        | JsonValueKind.False
        | JsonValueKind.Number -> Ok (e.GetRawText())
        | other -> Error [{ new IGQLError with member _.Message = $"'%O{other}' is not a JSON string, bool or number" }]
    | InlineConstant (IntValue i) -> Ok (i.ToString(CultureInfo.InvariantCulture))
    | InlineConstant (FloatValue f) -> Ok (f.ToString(CultureInfo.InvariantCulture))
    | InlineConstant (StringValue s) -> Ok s
    | InlineConstant (BooleanValue b) -> Ok (if b then "true" else "false")
    | other -> Error [{ new IGQLError with member _.Message = $"Cannot convert '%O{other}' to string" }]

let coerceEnumInput =
    function
    | EnumValue e -> Ok e
    | other -> Error [{ new IGQLError with member _.Message = $"Cannot convert '%O{other}' to enum" }]

/// Tries to resolve AST query input to bool.
let coerceBoolInput =
    function
    | Variable e when e.ValueKind = JsonValueKind.True -> Ok true
    | Variable e when e.ValueKind = JsonValueKind.False -> Ok false
    | InlineConstant (IntValue i) -> Ok (if i = 0L then false else true)
    | InlineConstant (FloatValue f) -> Ok (if f = 0. then false else true)
    | InlineConstant (StringValue s) ->
        match Boolean.TryParse(s) with
        | true, i -> Ok i
        | false, _ -> Error [{ new IGQLError with member _.Message = $"Cannot parse '%s{s}' as boolean" }]
    | InlineConstant (BooleanValue b) -> Ok b
    | other -> Error [{ new IGQLError with member _.Message = $"Cannot convert '%O{other}' to boolean" }]

/// Tries to resolve AST query input to provided generic type.
let private coerceIdInput input : GQLResult<'t> =
    match input with
    | Variable e when e.ValueKind = JsonValueKind.String -> Ok (downcast Convert.ChangeType(e.GetString() , typeof<'t>))
    | Variable e when e.ValueKind = JsonValueKind.Number -> Ok (downcast Convert.ChangeType(e.GetInt32() , typeof<'t>))
    | InlineConstant (IntValue i) -> Ok (downcast Convert.ChangeType(i, typeof<'t>))
    | InlineConstant (StringValue s) -> Ok (downcast Convert.ChangeType(s, typeof<'t>))
    | other -> Error [{ new IGQLError with member _.Message = $"Cannot convert '%O{other}' to ID" }]

/// Tries to resolve AST query input to URI.
let coerceUriInput =
    function
    | Variable e when e.ValueKind = JsonValueKind.String ->
        let s = e.GetString()
        match Uri.TryCreate(s, UriKind.RelativeOrAbsolute) with
        | true, uri -> Ok uri
        | false, _ -> Error [{ new IGQLError with member _.Message = $"Cannot parse '%s{s}' as URI" }]
    | InlineConstant (StringValue s) ->
        match Uri.TryCreate(s, UriKind.RelativeOrAbsolute) with
        | true, uri -> Ok uri
        | false, _ -> Error [{ new IGQLError with member _.Message = $"Cannot parse '%s{s}' as URI" }]
    | other -> Error [{ new IGQLError with member _.Message = $"Cannot convert '%O{other}' to URI" }]

/// Tries to resolve AST query input to DateTimeOffset.
let coerceDateTimeOffsetInput =
    function
    | Variable e when e.ValueKind = JsonValueKind.String ->
        let s = e.GetString()
        match DateTimeOffset.TryParse(s) with
        | true, date -> Ok date
        | false, _ -> Error [{ new IGQLError with member _.Message = $"Cannot parse '%s{s}' as DateTimeOffset" }]
    | InlineConstant (StringValue s) ->
        match DateTimeOffset.TryParse(s) with
        | true, date -> Ok date
        | false, _ -> Error [{ new IGQLError with member _.Message = $"Cannot parse '%s{s}' as DateTimeOffset" }]
    | other -> Error [{ new IGQLError with member _.Message = $"Cannot convert '%O{other}' to DateTimeOffset" }]

/// Tries to resolve AST query input to DateOnly.
let coerceDateOnlyInput =
    function
    | Variable e when e.ValueKind = JsonValueKind.String ->
        let s = e.GetString()
        match DateOnly.TryParse(s) with
        | true, date -> Ok date
        | false, _ -> Error [{ new IGQLError with member _.Message = $"Cannot parse '%s{s}' as DateOnly" }]
    | InlineConstant (StringValue s) ->
        match DateOnly.TryParse(s) with
        | true, date -> Ok date
        | false, _ -> Error [{ new IGQLError with member _.Message = $"Cannot parse '%s{s}' as DateOnly" }]
    | other -> Error [{ new IGQLError with member _.Message = $"Cannot convert '%O{other}' to DateOnly" }]

/// Tries to resolve AST query input to Guid.
let coerceGuidInput =
    function
    | Variable e when e.ValueKind = JsonValueKind.String ->
        let s = e.GetString()
        match Guid.TryParse(s) with
        | true, guid -> Ok guid
        | false, _ -> Error [{ new IGQLError with member _.Message = $"Cannot parse '%s{s}' as GUID" }]
    | InlineConstant (StringValue s) ->
        match Guid.TryParse(s) with
        | true, g -> Ok g
        | false, _ -> Error [{ new IGQLError with member _.Message = $"Cannot parse '%s{s}' as GUID" }]
    | other -> Error [{ new IGQLError with member _.Message = $"Cannot convert '%O{other}' to GUID" }]

/// Wraps a GraphQL type definition, allowing defining field/argument
/// to take option of provided value.
let Nullable(innerDef : #TypeDef<'Val>) : NullableDef<'Val> = upcast { NullableDefinition.OfType = innerDef }

/// Wraps a GraphQL type definition, allowing defining field/argument
/// to take collection of provided value.
let ListOf(innerDef : #TypeDef<'Val>) : ListOfDef<'Val, 'Seq> = upcast { ListOfDefinition.OfType = innerDef }

let private ignoreInputResolve (_ : unit) (input : 'T) = ()

let internal variableOrElse other value (variables : ImmutableDictionary<string, obj>) : GQLResult =
    match value with
    // TODO: Use FSharp.Collection.Immutable
    | VariableName variableName ->
        match variables.TryGetValue variableName with
        | true, value -> Ok value
        | false, _ -> Ok null
    | v -> other v

/// GraphQL type of int
let IntType : ScalarDefinition<int> =
    { Name = "Int"
      Description =
          Some
              "The `Int` scalar type represents non-fractional signed whole numeric values. Int can represent values between -(2^31) and 2^31 - 1."
      CoerceInput = coerceIntInput
      CoerceOutput = coerceIntValue }

/// GraphQL type of long
let LongType : ScalarDefinition<int64> =
    { Name = "Long"
      Description =
          Some
              "The `Long` scalar type represents non-fractional signed whole numeric values. Long can represent values between -(2^63) and 2^63 - 1."
      CoerceInput = coerceLongInput
      CoerceOutput = coerceLongValue }

/// GraphQL type of boolean
let BooleanType : ScalarDefinition<bool> =
    { Name = "Boolean"
      Description = Some "The `Boolean` scalar type represents `true` or `false`."
      CoerceInput = coerceBoolInput
      CoerceOutput = coerceBoolValue }

/// GraphQL type of float
let FloatType : ScalarDefinition<double> =
    { Name = "Float"
      Description =
          Some
              "The `Float` scalar type represents signed double-precision fractional values as specified by [IEEE 754](http://en.wikipedia.org/wiki/IEEE_floating_point)."
      CoerceInput = coerceFloatInput
      CoerceOutput = coerceFloatValue }

/// GraphQL type of string
let StringType : ScalarDefinition<string> =
    { Name = "String"
      Description =
          Some
              "The `String` scalar type represents textual data, represented as UTF-8 character sequences. The `String` type is most often used by GraphQL to represent free-form human-readable text."
      CoerceInput = coerceStringInput
      CoerceOutput = coerceStringValue }

/// GraphQL type for custom identifier
let IDType<'Val> : ScalarDefinition<'Val> =
    { Name = "ID"
      Description =
          Some
              "The `ID` scalar type represents a unique identifier, often used to refetch an object or as key for a cache. The `ID` type appears in a JSON response as a String; however, it is not intended to be human-readable. When expected as an input type, any string (such as `\"4\"`) or integer (such as `4`) input value will be accepted as an ID."
      CoerceInput = coerceIdInput
      CoerceOutput = coerceIdValue }

let ObjType : ScalarDefinition<obj> = {
        Name = "Object"
        Description =
           Some
              "The `Object` scalar type represents textual data, represented as UTF-8 character sequences. The `String` type is most often used by GraphQL to represent free-form human-readable text."
        CoerceInput = (fun o -> Ok (o))
        CoerceOutput = (fun o -> Ok (o))
    }

/// GraphQL type for System.Uri
let UriType : ScalarDefinition<Uri> =
    { Name = "URI"
      Description =
          Some
              "The `URI` scalar type represents a string resource identifier compatible with URI standard. The `URI` type appears in a JSON response as a String."
      CoerceInput = coerceUriInput
      CoerceOutput = coerceUriValue }

/// GraphQL type for System.DateTimeOffset
let DateTimeOffsetType : ScalarDefinition<DateTimeOffset> =
    { Name = "DateTimeOffset"
      Description =
          Some
              "The `DateTimeOffset` scalar type represents a Date value with Time component. The `DateTimeOffset` type appears in a JSON response as a String representation compatible with ISO-8601 format."
      CoerceInput = coerceDateTimeOffsetInput
      CoerceOutput = coerceDateTimeOffsetValue }

/// GraphQL type for System.DateOnly
let DateOnlyType : ScalarDefinition<DateOnly> =
    { Name = "DateOnly"
      Description =
          Some
              "The `DateOnly` scalar type represents a Date value without Time component. The `DateOnly` type appears in a JSON response as a `String` representation of full-date value as specified by [IETF 3339](https://www.ietf.org/rfc/rfc3339.txt)."
      CoerceInput = coerceDateOnlyInput
      CoerceOutput = coerceDateOnlyValue }

/// GraphQL type for System.Guid
let GuidType : ScalarDefinition<Guid> =
    { Name = "Guid"
      Description =
          Some
              "The `Guid` scalar type represents a Globaly Unique Identifier value. It's a 128-bit long byte key, that can be serialized to string."
      CoerceInput = coerceGuidInput
      CoerceOutput = coerceGuidValue }

/// GraphQL @include directive.
let IncludeDirective : DirectiveDef =
    { Name = "include"
      Description =
          Some "Directs the executor to include this field or fragment only when the `if` argument is true."
      Locations =
          DirectiveLocation.FIELD ||| DirectiveLocation.FRAGMENT_SPREAD ||| DirectiveLocation.INLINE_FRAGMENT
      Args =
          [| { InputFieldDefinition.Name = "if"
               Description = Some "Included when true."
               TypeDef = BooleanType
               DefaultValue = None
               ExecuteInput = variableOrElse (InlineConstant >> coerceBoolInput >> Result.map box) } |] }

/// GraphQL @skip directive.
let SkipDirective : DirectiveDef =
    { Name = "skip"
      Description = Some "Directs the executor to skip this field or fragment when the `if` argument is true."
      Locations =
          DirectiveLocation.FIELD ||| DirectiveLocation.FRAGMENT_SPREAD ||| DirectiveLocation.INLINE_FRAGMENT
      Args =
          [| { InputFieldDefinition.Name = "if"
               Description = Some "Skipped when true."
               TypeDef = BooleanType
               DefaultValue = None
               ExecuteInput = variableOrElse (InlineConstant >> coerceBoolInput >> Result.map box) } |] }

/// GraphQL @defer directive.
let DeferDirective : DirectiveDef =
    { Name = "defer"
      Description = Some "Defers the resolution of this field or fragment"
      Locations =
        DirectiveLocation.FIELD ||| DirectiveLocation.FRAGMENT_SPREAD ||| DirectiveLocation.INLINE_FRAGMENT ||| DirectiveLocation.FRAGMENT_DEFINITION
      Args = [||] }

/// GraphQL @stream directive.
let StreamDirective : DirectiveDef =
    { Name = "stream"
      Description = Some "Streams the resolution of this field or fragment"
      Locations =
        DirectiveLocation.FIELD ||| DirectiveLocation.FRAGMENT_SPREAD ||| DirectiveLocation.INLINE_FRAGMENT ||| DirectiveLocation.FRAGMENT_DEFINITION
      Args = [||] }

/// GraphQL @live directive.
let LiveDirective : DirectiveDef =
    { Name = "live"
      Description = Some "Subscribes for live updates of this field or fragment"
      Locations =
        DirectiveLocation.FIELD ||| DirectiveLocation.FRAGMENT_SPREAD ||| DirectiveLocation.INLINE_FRAGMENT ||| DirectiveLocation.FRAGMENT_DEFINITION
      Args = [||] }

let internal matchParameters (methodInfo : MethodInfo) (ctx : ResolveFieldContext) =
    methodInfo.GetParameters() |> Array.map (fun param -> ctx.Arg<obj>(param.Name))
let inline internal strip (fn : 'In -> 'Out) : obj -> obj = fun i -> upcast fn (i :?> 'In)

/// Common space for all definition helper methods.
[<AbstractClass; Sealed>]
type Define =

    /// <summary>
    /// Creates GraphQL type definition for user defined scalars.
    /// </summary>
    /// <param name="name">Type name. Must be unique in scope of the current schema.</param>
    /// <param name="coerceInput">Function used to resolve .NET object from GraphQL query AST.</param>
    /// <param name="coerceOutput">Function used to cross cast to .NET types.</param>
    /// <param name="description">Optional scalar description. Usefull for generating documentation.</param>
    static member Scalar(name : string, coerceInput : InputParameterValue -> GQLResult<'T>,
                         coerceOutput : obj -> GQLResult<'T>, ?description : string) : ScalarDefinition<'T> =
        { Name = name
          Description = description
          CoerceInput = coerceInput
          CoerceOutput = coerceOutput }

    /// <summary>
    /// Creates GraphQL type definition for user defined enums.
    /// </summary>
    /// <param name="name">Type name. Must be unique in scope of the current schema.</param>
    /// <param name="options">List of enum value cases.</param>
    /// <param name="description">Optional enum description. Usefull for generating documentation.</param>
    static member Enum(name : string, options : EnumValue<'Val> list, ?description : string) : EnumDef<'Val> =
        upcast { EnumDefinition.Name = name
                 Description = description
                 Options = options |> List.toArray }

    /// <summary>
    /// Creates a single enum option to be used as argument in <see cref="Schema.Enum"/>.
    /// </summary>
    /// <param name="name">Enum value name. Must be unique in scope of the defining enum.</param>
    /// <param name="value">
    /// .NET value associated with target enum value. All enum values are represented as strings in GraphQL type system.
    /// Enum value will be stringified using 'ToString()' method when passing to a client.
    /// </param>
    /// <param name="description">Optional enum value description. Usefull for generating documentation.</param>
    /// <param name="deprecationReason">If set, marks an enum value as deprecated.</param>
    static member EnumValue(name : string, value : 'Val, ?description : string, ?deprecationReason : string) : EnumValue<'Val> =
        { Name = name
          Description = description
          Value = value
          DeprecationReason = deprecationReason }

    /// <summary>
    /// Creates GraphQL custom output object type. It can be used as a valid output but not an input object
    /// (see <see cref="InputObject"/> for more details).
    /// </summary>
    /// <param name="name">Type name. Must be unique in scope of the current schema.</param>
    /// <param name="fieldsFn">
    /// Function which generates a list of fields defined by the current object. Usefull, when object defines recursive dependencies.
    /// </param>
    /// <param name="description">Optional object description. Usefull for generating documentation.</param>
    /// <param name="interfaces">
    /// List of implemented interfaces. Object must explicitly define all fields from all interfaces it implements.
    /// </param>
    /// <param name="isTypeOf">
    /// Optional function used to determine if provided .NET object instance matches current object definition.
    /// </param>
    static member Object(name : string, fieldsFn : unit -> FieldDef<'Val> list, ?description : string,
                         ?interfaces : InterfaceDef list, ?isTypeOf : obj -> bool) : ObjectDef<'Val> =
        upcast ({ Name = name
                  Description = description
                  FieldsFn =
                      lazy (fieldsFn()
                            |> List.map (fun f -> f.Name, f)
                            |> Map.ofList)
                  Implements = defaultArg (Option.map List.toArray interfaces) [||]
                  IsTypeOf = isTypeOf } : ObjectDefinition<'Val>)

    /// <summary>
    /// Creates GraphQL custom output object type. It can be used as a valid output but not an input object
    /// (see <see cref="InputObject"/> for more details).
    /// </summary>
    /// <param name="name">Type name. Must be unique in scope of the current schema.</param>
    /// <param name="fields">List of fields defined by the current object. </param>
    /// <param name="description">Optional object description. Usefull for generating documentation.</param>
    /// <param name="interfaces">
    /// List of implemented interfaces. Object must explicitly define all fields from all interfaces it implements.
    /// </param>
    /// <param name="isTypeOf">
    /// Optional function used to determine if provided .NET object instance matches current object definition.
    /// </param>
    static member Object(name : string, fields : FieldDef<'Val> list, ?description : string,
                         ?interfaces : InterfaceDef list, ?isTypeOf : obj -> bool) : ObjectDef<'Val> =
        upcast { ObjectDefinition.Name = name
                 Description = description
                 FieldsFn =
                     lazy (fields
                           |> List.map (fun f -> f.Name, f)
                           |> Map.ofList)
                 Implements = defaultArg (Option.map List.toArray interfaces) [||]
                 IsTypeOf = isTypeOf }

    /// <summary>
    /// Creates a custom GraphQL input object type. Unlike GraphQL objects, input objects are valid input types,
    /// that can be included in GraphQL query strings. Input object maps to a .NET type, which can be standard
    /// .NET class or struct, or a F# record.
    /// </summary>
    /// <param name="name">Type name. Must be unique in scope of the current schema.</param>
    /// <param name="fieldsFn">
    /// Function which generates a list of input fields defined by the current input object. Useful, when object defines recursive dependencies.
    /// </param>
    /// <param name="description">Optional input object description. Useful for generating documentation.</param>
    static member InputObject(name : string, fieldsFn : unit -> InputFieldDef list, ?description : string) : InputObjectDefinition<'Out> =
        { Name = name
          Fields = lazy (fieldsFn () |> List.toArray)
          Description = description }

    /// <summary>
    /// Creates a custom GraphQL input object type. Unlike GraphQL objects, input objects are valid input types,
    /// that can be included in GraphQL query strings. Input object maps to a .NET type, which can be standard
    /// .NET class or struct, or a F# record.
    /// </summary>
    /// <param name="name">Type name. Must be unique in scope of the current schema.</param>
    /// <param name="fields">List of input fields defined by the current input object. </param>
    /// <param name="description">Optional input object description. Useful for generating documentation.</param>
    static member InputObject(name : string, fields : InputFieldDef list, ?description : string) : InputObjectDefinition<'Out> =
        { Name = name
          Description = description
          Fields = lazy (fields |> List.toArray) }

    /// <summary>
    /// Creates the top level subscription object that holds all of the possible subscriptions as fields.
    /// </summary>
    /// <param name="name">Top level name. Must be unique in scope of the current schema.</param>
    /// <param name="fields">List of subscription fields to be defined for the schema. </param>
    /// <param name="description">Optional description. Usefull for generating documentation.</param>
    static member SubscriptionObject<'Val>(name: string, fields: SubscriptionFieldDef<'Val> list, ?description: string):SubscriptionObjectDefinition<'Val> =
        { Name = name
          Fields = (fields |> List.map (fun f -> f.Name, f) |> Map.ofList)
          Description = description }

    /// <summary>
    /// Creates field defined inside object types with automatically generated field resolve function.
    /// Field name must match object's property or field.
    /// </summary>
    /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
    /// <param name="typedef">GraphQL type definition of the current field's type.</param>
    /// <param name="description">Optional field description. Usefull for generating documentation.</param>
    /// <param name="args">Optional list of arguments used to parametrize field resolution.</param>
    /// <param name="deprecationReason">If set, marks current field as deprecated.</param>
    static member AutoField(name : string, typedef : #OutputDef<'Res>, ?description: string, ?args: InputFieldDef list, ?deprecationReason: string) : FieldDef<'Val> =
        upcast { FieldDefinition.Name = name
                 Description = description
                 TypeDef = typedef
                 Resolve = Resolve.defaultResolve<'Val, 'Res> name
                 Args = defaultArg args [] |> Array.ofList
                 DeprecationReason = deprecationReason
                 Metadata = Metadata.Empty
                 }

    /// <summary>
    /// Creates field defined inside interfaces. When used for objects may cause runtime exceptions due to
    /// lack of resolve function supplied. To use auto generated resolvers use <see cref="AutoField"/>.
    /// </summary>
    /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
    /// <param name="typedef">GraphQL type definition of the current field's type.</param>
    static member Field(name : string, typedef : OutputDef<'Res>) : FieldDef<'Val> =
        upcast { FieldDefinition.Name = name
                 Description = None
                 TypeDef = typedef
                 Resolve = Undefined
                 Args = [||]
                 DeprecationReason = None
                 Metadata = Metadata.Empty }

// GQLResult

    /// <summary>
    /// Creates field defined inside object type. Fields is marked as deprecated.
    /// </summary>
    /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
    /// <param name="typedef">GraphQL type definition of the current field's type.</param>
    /// <param name="description">Optional field description. Usefull for generating documentation.</param>
    /// <param name="args">List of field arguments used to parametrize resolve expression output.</param>
    /// <param name="resolve">Expression used to resolve value from defining object.</param>
    /// <param name="deprecationReason">Deprecation reason.</param>
    static member Field(name : string, typedef : OutputDef<'Res>, description : string, args : InputFieldDef list,
                        [<ReflectedDefinition(true)>] resolve : Expr<ResolveFieldContext -> 'Val -> 'Res GQLResult>,
                        ?deprecationReason : string) : FieldDef<'Val> =
        upcast { FieldDefinition.Name = name
                 Description = Some description
                 TypeDef = typedef
                 Resolve = Sync(typeof<'Val>, typeof<'Res>, resolve)
                 Args = args |> List.toArray
                 DeprecationReason = deprecationReason
                 Metadata = Metadata.Empty }

    /// <summary>
    /// Creates field defined inside object type.
    /// </summary>
    /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
    /// <param name="typedef">GraphQL type definition of the current field's type.</param>
    /// <param name="args">List of field arguments used to parametrize resolve expression output.</param>
    /// <param name="resolve">Expression used to resolve value from defining object.</param>
    static member Field(name : string, typedef : OutputDef<'Res>, args : InputFieldDef list,
                        [<ReflectedDefinition(true)>] resolve : Expr<ResolveFieldContext -> 'Val -> 'Res GQLResult>,
                        ?deprecationReason : string) : FieldDef<'Val> =
        upcast { FieldDefinition.Name = name
                 Description = None
                 TypeDef = typedef
                 Resolve = Sync(typeof<'Val>, typeof<'Res>, resolve)
                 Args = args |> List.toArray
                 DeprecationReason = deprecationReason
                 Metadata = Metadata.Empty }

    /// <summary>
    /// Creates field defined inside object type.
    /// </summary>
    /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
    /// <param name="typedef">GraphQL type definition of the current field's type.</param>
    /// <param name="description">Optional field description. Usefull for generating documentation.</param>
    /// <param name="resolve">Expression used to resolve value from defining object.</param>
    static member Field(name : string, typedef : OutputDef<'Res>, description : string,
                        [<ReflectedDefinition(true)>] resolve : Expr<ResolveFieldContext -> 'Val -> 'Res GQLResult>,
                        ?deprecationReason : string) : FieldDef<'Val> =

        upcast { FieldDefinition.Name = name
                 Description = Some description
                 TypeDef = typedef
                 Resolve = Sync(typeof<'Val>, typeof<'Res>, resolve)
                 Args = [||]
                 DeprecationReason = deprecationReason
                 Metadata = Metadata.Empty }

    /// <summary>
    /// Creates field defined inside object type.
    /// </summary>
    /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
    /// <param name="typedef">GraphQL type definition of the current field's type.</param>
    /// <param name="resolve">Expression used to resolve value from defining object.</param>
    static member Field(name : string, typedef : OutputDef<'Res>,
                        [<ReflectedDefinition(true)>] resolve : Expr<ResolveFieldContext -> 'Val -> 'Res GQLResult>,
                        ?deprecationReason : string) : FieldDef<'Val> =
        upcast { FieldDefinition.Name = name
                 Description = None
                 TypeDef = typedef
                 Resolve = Sync(typeof<'Val>, typeof<'Res>, resolve)
                 Args = [||]
                 DeprecationReason = deprecationReason
                 Metadata = Metadata.Empty }

// GQLResult option

    /// <summary>
    /// Creates field defined inside object type.
    /// </summary>
    /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
    /// <param name="typedef">GraphQL type definition of the current field's type.</param>
    /// <param name="description">Optional field description. Usefull for generating documentation.</param>
    /// <param name="args">List of field arguments used to parametrize resolve expression output.</param>
    /// <param name="resolve">Expression used to resolve value from defining object.</param>
    /// <param name="deprecationReason">Deprecation reason.</param>
    static member Field(name : string, typedef : NullableDef<'Res>, description : string, args : InputFieldDef list,
                        resolve : ResolveFieldContext -> 'Val -> 'Res option GQLResult,
                        ?deprecationReason : string) : FieldDef<'Val> =
        let resolver ctx' value' : GQLResult<'Res> = resolve ctx' value' |> Result.map (Option.map box >> Option.toObj >> unbox)
        Define.Field(name, typedef, description, args, resolver, ?deprecationReason = deprecationReason)

    /// <summary>
    /// Creates field defined inside object type.
    /// </summary>
    /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
    /// <param name="typedef">GraphQL type definition of the current field's type.</param>
    /// <param name="description">Optional field description. Usefull for generating documentation.</param>
    /// <param name="resolve">Expression used to resolve value from defining object.</param>
    /// <param name="deprecationReason">Deprecation reason.</param>
    static member Field(name : string, typedef : NullableDef<'Res>, description : string,
                        resolve : ResolveFieldContext -> 'Val -> 'Res option GQLResult,
                        ?deprecationReason : string) : FieldDef<'Val> =
        let resolver ctx' value' : GQLResult<'Res> = resolve ctx' value' |> Result.map (Option.map box >> Option.toObj >> unbox)
        Define.Field(name, typedef, description, resolver, ?deprecationReason = deprecationReason)

    /// <summary>
    /// Creates field defined inside object type.
    /// </summary>
    /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
    /// <param name="typedef">GraphQL type definition of the current field's type.</param>
    /// <param name="description">Optional field description. Usefull for generating documentation.</param>
    /// <param name="args">List of field arguments used to parametrize resolve expression output.</param>
    /// <param name="resolve">Expression used to resolve value from defining object.</param>
    /// <param name="deprecationReason">Deprecation reason.</param>
    static member Field(name : string, typedef : NullableDef<'Res>, args : InputFieldDef list,
                        resolve : ResolveFieldContext -> 'Val -> 'Res option GQLResult,
                        ?deprecationReason : string) : FieldDef<'Val> =
        let resolver ctx' value' : GQLResult<'Res> = resolve ctx' value' |> Result.map (Option.map box >> Option.toObj >> unbox)
        Define.Field(name, typedef, args, resolver, ?deprecationReason = deprecationReason)

    /// <summary>
    /// Creates field defined inside object type.
    /// </summary>
    /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
    /// <param name="typedef">GraphQL type definition of the current field's type.</param>
    /// <param name="description">Optional field description. Usefull for generating documentation.</param>
    /// <param name="resolve">Expression used to resolve value from defining object.</param>
    /// <param name="deprecationReason">Deprecation reason.</param>
    static member Field(name : string, typedef : NullableDef<'Res>,
                        resolve : ResolveFieldContext -> 'Val -> 'Res option GQLResult,
                        ?deprecationReason : string) : FieldDef<'Val> =
        let resolver ctx' value' : GQLResult<'Res> = resolve ctx' value' |> Result.map (Option.map box >> Option.toObj >> unbox)
        Define.Field(name, typedef, resolver, ?deprecationReason = deprecationReason)

// GQLResult voption

    /// <summary>
    /// Creates field defined inside object type.
    /// </summary>
    /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
    /// <param name="typedef">GraphQL type definition of the current field's type.</param>
    /// <param name="description">Optional field description. Usefull for generating documentation.</param>
    /// <param name="args">List of field arguments used to parametrize resolve expression output.</param>
    /// <param name="resolve">Expression used to resolve value from defining object.</param>
    /// <param name="deprecationReason">Deprecation reason.</param>
    static member Field(name : string, typedef : NullableDef<'Res>, description : string, args : InputFieldDef list,
                        resolve : ResolveFieldContext -> 'Val -> 'Res voption GQLResult,
                        ?deprecationReason : string) : FieldDef<'Val> =
        let resolver ctx' value' : GQLResult<'Res> = resolve ctx' value' |> Result.map (ValueOption.map box >> ValueOption.toObj >> unbox)
        Define.Field(name, typedef, description, args, resolver, ?deprecationReason = deprecationReason)

    /// <summary>
    /// Creates field defined inside object type.
    /// </summary>
    /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
    /// <param name="typedef">GraphQL type definition of the current field's type.</param>
    /// <param name="description">Optional field description. Usefull for generating documentation.</param>
    /// <param name="resolve">Expression used to resolve value from defining object.</param>
    /// <param name="deprecationReason">Deprecation reason.</param>
    static member Field(name : string, typedef : NullableDef<'Res>, description : string,
                        resolve : ResolveFieldContext -> 'Val -> 'Res voption GQLResult,
                        ?deprecationReason : string) : FieldDef<'Val> =
        let resolver ctx' value' : GQLResult<'Res> = resolve ctx' value' |> Result.map (ValueOption.map box >> ValueOption.toObj >> unbox)
        Define.Field(name, typedef, description, resolver, ?deprecationReason = deprecationReason)

    /// <summary>
    /// Creates field defined inside object type.
    /// </summary>
    /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
    /// <param name="typedef">GraphQL type definition of the current field's type.</param>
    /// <param name="description">Optional field description. Usefull for generating documentation.</param>
    /// <param name="args">List of field arguments used to parametrize resolve expression output.</param>
    /// <param name="resolve">Expression used to resolve value from defining object.</param>
    /// <param name="deprecationReason">Deprecation reason.</param>
    static member Field(name : string, typedef : NullableDef<'Res>, args : InputFieldDef list,
                        resolve : ResolveFieldContext -> 'Val -> 'Res voption GQLResult,
                        ?deprecationReason : string) : FieldDef<'Val> =
        let resolver ctx' value' : GQLResult<'Res> = resolve ctx' value' |> Result.map (ValueOption.map box >> ValueOption.toObj >> unbox)
        Define.Field(name, typedef, args, resolver, ?deprecationReason = deprecationReason)

    /// <summary>
    /// Creates field defined inside object type.
    /// </summary>
    /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
    /// <param name="typedef">GraphQL type definition of the current field's type.</param>
    /// <param name="description">Optional field description. Usefull for generating documentation.</param>
    /// <param name="resolve">Expression used to resolve value from defining object.</param>
    /// <param name="deprecationReason">Deprecation reason.</param>
    static member Field(name : string, typedef : NullableDef<'Res>,
                        resolve : ResolveFieldContext -> 'Val -> 'Res voption GQLResult,
                        ?deprecationReason : string) : FieldDef<'Val> =
        let resolver ctx' value' : GQLResult<'Res> = resolve ctx' value' |> Result.map (ValueOption.map box >> ValueOption.toObj >> unbox)
        Define.Field(name, typedef, resolver, ?deprecationReason = deprecationReason)

// option

    /// <summary>
    /// Creates field defined inside object type.
    /// </summary>
    /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
    /// <param name="typedef">GraphQL type definition of the current field's type.</param>
    /// <param name="description">Optional field description. Usefull for generating documentation.</param>
    /// <param name="args">List of field arguments used to parametrize resolve expression output.</param>
    /// <param name="resolve">Expression used to resolve value from defining object.</param>
    /// <param name="deprecationReason">Deprecation reason.</param>
    static member Field(name : string, typedef : NullableDef<'Res>, description : string, args : InputFieldDef list,
                        resolve : ResolveFieldContext -> 'Val -> 'Res option,
                        ?deprecationReason : string) : FieldDef<'Val> =
        let resolver ctx' value' : GQLResult<'Res> = resolve ctx' value' |> Option.map box |> Option.toObj |> unbox |> Ok
        Define.Field(name, typedef, description, args, resolver, ?deprecationReason = deprecationReason)

    /// <summary>
    /// Creates field defined inside object type.
    /// </summary>
    /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
    /// <param name="typedef">GraphQL type definition of the current field's type.</param>
    /// <param name="description">Optional field description. Usefull for generating documentation.</param>
    /// <param name="resolve">Expression used to resolve value from defining object.</param>
    /// <param name="deprecationReason">Deprecation reason.</param>
    static member Field(name : string, typedef : NullableDef<'Res>, description : string,
                        resolve : ResolveFieldContext -> 'Val -> 'Res option,
                        ?deprecationReason : string) : FieldDef<'Val> =
        let resolver ctx' value' : GQLResult<'Res> = resolve ctx' value' |> Option.map box |> Option.toObj |> unbox |> Ok
        Define.Field(name, typedef, description, resolver, ?deprecationReason = deprecationReason)

    /// <summary>
    /// Creates field defined inside object type.
    /// </summary>
    /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
    /// <param name="typedef">GraphQL type definition of the current field's type.</param>
    /// <param name="description">Optional field description. Usefull for generating documentation.</param>
    /// <param name="args">List of field arguments used to parametrize resolve expression output.</param>
    /// <param name="resolve">Expression used to resolve value from defining object.</param>
    /// <param name="deprecationReason">Deprecation reason.</param>
    static member Field(name : string, typedef : NullableDef<'Res>, args : InputFieldDef list,
                        resolve : ResolveFieldContext -> 'Val -> 'Res option,
                        ?deprecationReason : string) : FieldDef<'Val> =
        let resolver ctx' value' : GQLResult<'Res> = resolve ctx' value' |> Option.map box |> Option.toObj |> unbox |> Ok
        Define.Field(name, typedef, args, resolver, ?deprecationReason = deprecationReason)

    /// <summary>
    /// Creates field defined inside object type.
    /// </summary>
    /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
    /// <param name="typedef">GraphQL type definition of the current field's type.</param>
    /// <param name="description">Optional field description. Usefull for generating documentation.</param>
    /// <param name="resolve">Expression used to resolve value from defining object.</param>
    /// <param name="deprecationReason">Deprecation reason.</param>
    static member Field(name : string, typedef : NullableDef<'Res>,
                        resolve : ResolveFieldContext -> 'Val -> 'Res option,
                        ?deprecationReason : string) : FieldDef<'Val> =
        let resolver ctx' value' : GQLResult<'Res> = resolve ctx' value' |> Option.map box |> Option.toObj |> unbox |> Ok
        Define.Field(name, typedef, resolver, ?deprecationReason = deprecationReason)

// voption

    /// <summary>
    /// Creates field defined inside object type.
    /// </summary>
    /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
    /// <param name="typedef">GraphQL type definition of the current field's type.</param>
    /// <param name="description">Optional field description. Usefull for generating documentation.</param>
    /// <param name="args">List of field arguments used to parametrize resolve expression output.</param>
    /// <param name="resolve">Expression used to resolve value from defining object.</param>
    /// <param name="deprecationReason">Deprecation reason.</param>
    static member Field(name : string, typedef : NullableDef<'Res>, description : string, args : InputFieldDef list,
                        resolve : ResolveFieldContext -> 'Val -> 'Res voption,
                        ?deprecationReason : string) : FieldDef<'Val> =
        let resolver ctx' value' : GQLResult<'Res> = resolve ctx' value' |> ValueOption.map box |> ValueOption.toObj |> unbox |> Ok
        Define.Field(name, typedef, description, args, resolver, ?deprecationReason = deprecationReason)

    /// <summary>
    /// Creates field defined inside object type.
    /// </summary>
    /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
    /// <param name="typedef">GraphQL type definition of the current field's type.</param>
    /// <param name="description">Optional field description. Usefull for generating documentation.</param>
    /// <param name="resolve">Expression used to resolve value from defining object.</param>
    /// <param name="deprecationReason">Deprecation reason.</param>
    static member Field(name : string, typedef : NullableDef<'Res>, description : string,
                        resolve : ResolveFieldContext -> 'Val -> 'Res voption,
                        ?deprecationReason : string) : FieldDef<'Val> =
        let resolver ctx' value' : GQLResult<'Res> = resolve ctx' value' |> ValueOption.map box |> ValueOption.toObj |> unbox |> Ok
        Define.Field(name, typedef, description, resolver, ?deprecationReason = deprecationReason)

    /// <summary>
    /// Creates field defined inside object type.
    /// </summary>
    /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
    /// <param name="typedef">GraphQL type definition of the current field's type.</param>
    /// <param name="description">Optional field description. Usefull for generating documentation.</param>
    /// <param name="args">List of field arguments used to parametrize resolve expression output.</param>
    /// <param name="resolve">Expression used to resolve value from defining object.</param>
    /// <param name="deprecationReason">Deprecation reason.</param>
    static member Field(name : string, typedef : NullableDef<'Res>, args : InputFieldDef list,
                        resolve : ResolveFieldContext -> 'Val -> 'Res voption,
                        ?deprecationReason : string) : FieldDef<'Val> =
        let resolver ctx' value' : GQLResult<'Res> = resolve ctx' value' |> ValueOption.map box |> ValueOption.toObj |> unbox |> Ok
        Define.Field(name, typedef, args, resolver, ?deprecationReason = deprecationReason)

    /// <summary>
    /// Creates field defined inside object type.
    /// </summary>
    /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
    /// <param name="typedef">GraphQL type definition of the current field's type.</param>
    /// <param name="description">Optional field description. Usefull for generating documentation.</param>
    /// <param name="resolve">Expression used to resolve value from defining object.</param>
    /// <param name="deprecationReason">Deprecation reason.</param>
    static member Field(name : string, typedef : NullableDef<'Res>,
                        resolve : ResolveFieldContext -> 'Val -> 'Res voption,
                        ?deprecationReason : string) : FieldDef<'Val> =
        let resolver ctx' value' : GQLResult<'Res> = resolve ctx' value' |> ValueOption.map box |> ValueOption.toObj |> unbox |> Ok
        Define.Field(name, typedef, resolver, ?deprecationReason = deprecationReason)

// value

    /// <summary>
    /// Creates field defined inside object type.
    /// </summary>
    /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
    /// <param name="typedef">GraphQL type definition of the current field's type.</param>
    /// <param name="description">Optional field description. Usefull for generating documentation.</param>
    /// <param name="args">List of field arguments used to parametrize resolve expression output.</param>
    /// <param name="resolve">Expression used to resolve value from defining object.</param>
    /// <param name="deprecationReason">Deprecation reason.</param>
    static member Field(name : string, typedef : OutputDef<'Res>, description : string, args : InputFieldDef list,
                        resolve : ResolveFieldContext -> 'Val -> 'Res,
                        ?deprecationReason : string) : FieldDef<'Val> =
        let resolver ctx' value' : GQLResult<'Res> = resolve ctx' value' |> Ok
        Define.Field(name, typedef, description, args, resolver, ?deprecationReason = deprecationReason)

    /// <summary>
    /// Creates field defined inside object type.
    /// </summary>
    /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
    /// <param name="typedef">GraphQL type definition of the current field's type.</param>
    /// <param name="description">Optional field description. Usefull for generating documentation.</param>
    /// <param name="resolve">Expression used to resolve value from defining object.</param>
    /// <param name="deprecationReason">Deprecation reason.</param>
    static member Field(name : string, typedef : OutputDef<'Res>, description : string,
                        resolve : ResolveFieldContext -> 'Val -> 'Res,
                        ?deprecationReason : string) : FieldDef<'Val> =
        let resolver ctx' value' : GQLResult<'Res> = resolve ctx' value' |> Ok
        Define.Field(name, typedef, description, resolver, ?deprecationReason = deprecationReason)

    /// <summary>
    /// Creates field defined inside object type.
    /// </summary>
    /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
    /// <param name="typedef">GraphQL type definition of the current field's type.</param>
    /// <param name="description">Optional field description. Usefull for generating documentation.</param>
    /// <param name="args">List of field arguments used to parametrize resolve expression output.</param>
    /// <param name="resolve">Expression used to resolve value from defining object.</param>
    /// <param name="deprecationReason">Deprecation reason.</param>
    static member Field(name : string, typedef : OutputDef<'Res>, args : InputFieldDef list,
                        resolve : ResolveFieldContext -> 'Val -> 'Res,
                        ?deprecationReason : string) : FieldDef<'Val> =
        let resolver ctx' value' : GQLResult<'Res> = resolve ctx' value' |> Ok
        Define.Field(name, typedef, args, resolver, ?deprecationReason = deprecationReason)

    /// <summary>
    /// Creates field defined inside object type.
    /// </summary>
    /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
    /// <param name="typedef">GraphQL type definition of the current field's type.</param>
    /// <param name="description">Optional field description. Usefull for generating documentation.</param>
    /// <param name="resolve">Expression used to resolve value from defining object.</param>
    /// <param name="deprecationReason">Deprecation reason.</param>
    static member Field(name : string, typedef : OutputDef<'Res>,
                        resolve : ResolveFieldContext -> 'Val -> 'Res,
                        ?deprecationReason : string) : FieldDef<'Val> =
        let resolver ctx' value' : GQLResult<'Res> = resolve ctx' value' |> Ok
        Define.Field(name, typedef, resolver, ?deprecationReason = deprecationReason)


// Async GQLResult

    /// <summary>
    /// Creates field defined inside object type with asynchronously resolved value.
    /// </summary>
    /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
    /// <param name="typedef">GraphQL type definition of the current field's type.</param>
    /// <param name="resolve">Expression used to resolve value from defining object.</param>
    static member AsyncField(name : string, typedef : OutputDef<'Res>,
                             [<ReflectedDefinition(true)>] resolve : Expr<ResolveFieldContext -> 'Val -> Async<'Res GQLResult>>,
                             ?deprecationReason : string) : FieldDef<'Val> =
        upcast { FieldDefinition.Name = name
                 Description = None
                 TypeDef = typedef
                 Resolve = Async(typeof<'Val>, typeof<'Res>, resolve)
                 Args = [||]
                 DeprecationReason = deprecationReason
                 Metadata = Metadata.Empty }

    /// <summary>
    /// Creates field defined inside object type with asynchronously resolved value.
    /// </summary>
    /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
    /// <param name="typedef">GraphQL type definition of the current field's type.</param>
    /// <param name="description">Optional field description. Usefull for generating documentation.</param>
    /// <param name="resolve">Expression used to resolve value from defining object.</param>
    static member AsyncField(name : string, typedef : OutputDef<'Res>, description : string,
                             [<ReflectedDefinition(true)>] resolve : Expr<ResolveFieldContext -> 'Val -> Async<'Res GQLResult>>,
                             ?deprecationReason : string) : FieldDef<'Val> =
        upcast { FieldDefinition.Name = name
                 Description = Some description
                 TypeDef = typedef
                 Resolve = Async(typeof<'Val>, typeof<'Res>, resolve)
                 Args = [||]
                 DeprecationReason = deprecationReason
                 Metadata = Metadata.Empty }

    /// <summary>
    /// Creates field defined inside object type with asynchronously resolved value.
    /// </summary>
    /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
    /// <param name="typedef">GraphQL type definition of the current field's type.</param>
    /// <param name="args">List of field arguments used to parametrize resolve expression output.</param>
    /// <param name="resolve">Expression used to resolve value from defining object.</param>
    /// <param name="deprecationReason">Deprecation reason.</param>
    static member AsyncField(name : string, typedef : OutputDef<'Res>, args : InputFieldDef list,
                             [<ReflectedDefinition(true)>] resolve : Expr<ResolveFieldContext -> 'Val -> Async<'Res GQLResult>>,
                             ?deprecationReason : string) : FieldDef<'Val> =
        upcast { FieldDefinition.Name = name
                 Description = None
                 TypeDef = typedef
                 Resolve = Async(typeof<'Val>, typeof<'Res>, resolve)
                 Args = args |> List.toArray
                 DeprecationReason = deprecationReason
                 Metadata = Metadata.Empty }

    /// <summary>
    /// Creates field defined inside object type with asynchronously resolved value.
    /// </summary>
    /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
    /// <param name="typedef">GraphQL type definition of the current field's type.</param>
    /// <param name="description">Optional field description. Usefull for generating documentation.</param>
    /// <param name="args">List of field arguments used to parametrize resolve expression output.</param>
    /// <param name="resolve">Expression used to resolve value from defining object.</param>
    /// <param name="deprecationReason">Deprecation reason.</param>
    static member AsyncField(name : string, typedef : OutputDef<'Res>, description : string,
                             args : InputFieldDef list,
                             [<ReflectedDefinition(true)>] resolve : Expr<ResolveFieldContext -> 'Val -> Async<'Res GQLResult>>,
                             ?deprecationReason : string) : FieldDef<'Val> =
        upcast { FieldDefinition.Name = name
                 Description = Some description
                 TypeDef = typedef
                 Resolve = Async(typeof<'Val>, typeof<'Res>, resolve)
                 Args = args |> List.toArray
                 DeprecationReason = deprecationReason
                 Metadata = Metadata.Empty }

// Async GQLResult option

    /// <summary>
    /// Creates field defined inside object type with asynchronously resolved value.
    /// </summary>
    /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
    /// <param name="typedef">GraphQL type definition of the current field's type.</param>
    /// <param name="resolve">Expression used to resolve value from defining object.</param>
    static member AsyncField(name : string, typedef : NullableDef<'Res>,
                             resolve : ResolveFieldContext -> 'Val -> Async<'Res option GQLResult>,
                             ?deprecationReason : string) : FieldDef<'Val> =
        let resolver ctx' value' : GQLResult<'Res> Async = async {
            let! result = resolve ctx' value'
            return result |> Result.map (Option.map box >> Option.toObj |> unbox)
        }
        Define.AsyncField (name, typedef, resolver, ?deprecationReason = deprecationReason)

    /// <summary>
    /// Creates field defined inside object type with asynchronously resolved value.
    /// </summary>
    /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
    /// <param name="typedef">GraphQL type definition of the current field's type.</param>
    /// <param name="description">Optional field description. Usefull for generating documentation.</param>
    /// <param name="resolve">Expression used to resolve value from defining object.</param>
    static member AsyncField(name : string, typedef : NullableDef<'Res>, description : string,
                             resolve : ResolveFieldContext -> 'Val -> Async<'Res option GQLResult>,
                             ?deprecationReason : string) : FieldDef<'Val> =
        let resolver ctx' value' : GQLResult<'Res> Async = async {
            let! result = resolve ctx' value'
            return result |> Result.map (Option.map box >> Option.toObj |> unbox)
        }
        Define.AsyncField (name, typedef, description, resolver, ?deprecationReason = deprecationReason)

    /// <summary>
    /// Creates field defined inside object type with asynchronously resolved value.
    /// </summary>
    /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
    /// <param name="typedef">GraphQL type definition of the current field's type.</param>
    /// <param name="args">List of field arguments used to parametrize resolve expression output.</param>
    /// <param name="resolve">Expression used to resolve value from defining object.</param>
    /// <param name="deprecationReason">Deprecation reason.</param>
    static member AsyncField(name : string, typedef : NullableDef<'Res>, args : InputFieldDef list,
                             resolve : ResolveFieldContext -> 'Val -> Async<'Res option GQLResult>,
                             ?deprecationReason : string) : FieldDef<'Val> =
        let resolver ctx' value' : GQLResult<'Res> Async = async {
            let! result = resolve ctx' value'
            return result |> Result.map (Option.map box >> Option.toObj |> unbox)
        }
        Define.AsyncField (name, typedef, args, resolver, ?deprecationReason = deprecationReason)

    /// <summary>
    /// Creates field defined inside object type with asynchronously resolved value.
    /// </summary>
    /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
    /// <param name="typedef">GraphQL type definition of the current field's type.</param>
    /// <param name="description">Optional field description. Usefull for generating documentation.</param>
    /// <param name="args">List of field arguments used to parametrize resolve expression output.</param>
    /// <param name="resolve">Expression used to resolve value from defining object.</param>
    /// <param name="deprecationReason">Deprecation reason.</param>
    static member AsyncField(name : string, typedef : NullableDef<'Res>, description : string,
                             args : InputFieldDef list,
                             resolve : ResolveFieldContext -> 'Val -> Async<'Res option GQLResult>,
                             ?deprecationReason : string) : FieldDef<'Val> =
        let resolver ctx' value' : GQLResult<'Res> Async = async {
            let! result = resolve ctx' value'
            return result |> Result.map (Option.map box >> Option.toObj |> unbox)
        }
        Define.AsyncField (name, typedef, description, args, resolver, ?deprecationReason = deprecationReason)

// Async GQLResult voption

    /// <summary>
    /// Creates field defined inside object type with asynchronously resolved value.
    /// </summary>
    /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
    /// <param name="typedef">GraphQL type definition of the current field's type.</param>
    /// <param name="resolve">Expression used to resolve value from defining object.</param>
    static member AsyncField(name : string, typedef : NullableDef<'Res>,
                             resolve : ResolveFieldContext -> 'Val -> Async<'Res voption GQLResult>,
                             ?deprecationReason : string) : FieldDef<'Val> =
        let resolver ctx' value' : GQLResult<'Res> Async = async {
            let! result = resolve ctx' value'
            return result |> Result.map (ValueOption.map box >> ValueOption.toObj |> unbox)
        }
        Define.AsyncField (name, typedef, resolver, ?deprecationReason = deprecationReason)

    /// <summary>
    /// Creates field defined inside object type with asynchronously resolved value.
    /// </summary>
    /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
    /// <param name="typedef">GraphQL type definition of the current field's type.</param>
    /// <param name="description">Optional field description. Usefull for generating documentation.</param>
    /// <param name="resolve">Expression used to resolve value from defining object.</param>
    static member AsyncField(name : string, typedef : NullableDef<'Res>, description : string,
                             resolve : ResolveFieldContext -> 'Val -> Async<'Res voption GQLResult>,
                             ?deprecationReason : string) : FieldDef<'Val> =
        let resolver ctx' value' : GQLResult<'Res> Async = async {
            let! result = resolve ctx' value'
            return result |> Result.map (ValueOption.map box >> ValueOption.toObj |> unbox)
        }
        Define.AsyncField (name, typedef, description, resolver, ?deprecationReason = deprecationReason)

    /// <summary>
    /// Creates field defined inside object type with asynchronously resolved value.
    /// </summary>
    /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
    /// <param name="typedef">GraphQL type definition of the current field's type.</param>
    /// <param name="args">List of field arguments used to parametrize resolve expression output.</param>
    /// <param name="resolve">Expression used to resolve value from defining object.</param>
    /// <param name="deprecationReason">Deprecation reason.</param>
    static member AsyncField(name : string, typedef : NullableDef<'Res>, args : InputFieldDef list,
                             resolve : ResolveFieldContext -> 'Val -> Async<'Res voption GQLResult>,
                             ?deprecationReason : string) : FieldDef<'Val> =
        let resolver ctx' value' : GQLResult<'Res> Async = async {
            let! result = resolve ctx' value'
            return result |> Result.map (ValueOption.map box >> ValueOption.toObj |> unbox)
        }
        Define.AsyncField (name, typedef, args, resolver, ?deprecationReason = deprecationReason)

    /// <summary>
    /// Creates field defined inside object type with asynchronously resolved value.
    /// </summary>
    /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
    /// <param name="typedef">GraphQL type definition of the current field's type.</param>
    /// <param name="description">Optional field description. Usefull for generating documentation.</param>
    /// <param name="args">List of field arguments used to parametrize resolve expression output.</param>
    /// <param name="resolve">Expression used to resolve value from defining object.</param>
    /// <param name="deprecationReason">Deprecation reason.</param>
    static member AsyncField(name : string, typedef : NullableDef<'Res>, description : string,
                             args : InputFieldDef list,
                             resolve : ResolveFieldContext -> 'Val -> Async<'Res voption GQLResult>,
                             ?deprecationReason : string) : FieldDef<'Val> =
        let resolver ctx' value' : GQLResult<'Res> Async = async {
            let! result = resolve ctx' value'
            return result |> Result.map (ValueOption.map box >> ValueOption.toObj |> unbox)
        }
        Define.AsyncField (name, typedef, description, args, resolver, ?deprecationReason = deprecationReason)

// Async option

    /// <summary>
    /// Creates field defined inside object type with asynchronously resolved value.
    /// </summary>
    /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
    /// <param name="typedef">GraphQL type definition of the current field's type.</param>
    /// <param name="resolve">Expression used to resolve value from defining object.</param>
    static member AsyncField(name : string, typedef : NullableDef<'Res>,
                             resolve : ResolveFieldContext -> 'Val -> Async<'Res option>,
                             ?deprecationReason : string) : FieldDef<'Val> =
        let resolver ctx' value' : GQLResult<'Res> Async = async {
            let! result = resolve ctx' value'
            return result |> Option.map box |> Option.toObj |> unbox |> Ok
        }
        Define.AsyncField (name, typedef, resolver, ?deprecationReason = deprecationReason)

    /// <summary>
    /// Creates field defined inside object type with asynchronously resolved value.
    /// </summary>
    /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
    /// <param name="typedef">GraphQL type definition of the current field's type.</param>
    /// <param name="description">Optional field description. Usefull for generating documentation.</param>
    /// <param name="resolve">Expression used to resolve value from defining object.</param>
    static member AsyncField(name : string, typedef : NullableDef<'Res>, description : string,
                             resolve : ResolveFieldContext -> 'Val -> Async<'Res option>,
                             ?deprecationReason : string) : FieldDef<'Val> =
        let resolver ctx' value' : GQLResult<'Res> Async = async {
            let! result = resolve ctx' value'
            return result |> Option.map box |> Option.toObj |> unbox |> Ok
        }
        Define.AsyncField (name, typedef, description, resolver, ?deprecationReason = deprecationReason)

    /// <summary>
    /// Creates field defined inside object type with asynchronously resolved value.
    /// </summary>
    /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
    /// <param name="typedef">GraphQL type definition of the current field's type.</param>
    /// <param name="args">List of field arguments used to parametrize resolve expression output.</param>
    /// <param name="resolve">Expression used to resolve value from defining object.</param>
    /// <param name="deprecationReason">Deprecation reason.</param>
    static member AsyncField(name : string, typedef : NullableDef<'Res>, args : InputFieldDef list,
                             resolve : ResolveFieldContext -> 'Val -> Async<'Res option>,
                             ?deprecationReason : string) : FieldDef<'Val> =
        let resolver ctx' value' : GQLResult<'Res> Async = async {
            let! result = resolve ctx' value'
            return result |> Option.map box |> Option.toObj |> unbox |> Ok
        }
        Define.AsyncField (name, typedef, args, resolver, ?deprecationReason = deprecationReason)

    /// <summary>
    /// Creates field defined inside object type with asynchronously resolved value.
    /// </summary>
    /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
    /// <param name="typedef">GraphQL type definition of the current field's type.</param>
    /// <param name="description">Optional field description. Usefull for generating documentation.</param>
    /// <param name="args">List of field arguments used to parametrize resolve expression output.</param>
    /// <param name="resolve">Expression used to resolve value from defining object.</param>
    /// <param name="deprecationReason">Deprecation reason.</param>
    static member AsyncField(name : string, typedef : NullableDef<'Res>, description : string,
                             args : InputFieldDef list,
                             resolve : ResolveFieldContext -> 'Val -> Async<'Res option>,
                             ?deprecationReason : string) : FieldDef<'Val> =
        let resolver ctx' value' : GQLResult<'Res> Async = async {
            let! result = resolve ctx' value'
            return result |> Option.map box |> Option.toObj |> unbox |> Ok
        }
        Define.AsyncField (name, typedef, description, args, resolver, ?deprecationReason = deprecationReason)

// Async voption

    /// <summary>
    /// Creates field defined inside object type with asynchronously resolved value.
    /// </summary>
    /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
    /// <param name="typedef">GraphQL type definition of the current field's type.</param>
    /// <param name="resolve">Expression used to resolve value from defining object.</param>
    static member AsyncField(name : string, typedef : NullableDef<'Res>,
                             resolve : ResolveFieldContext -> 'Val -> Async<'Res voption>,
                             ?deprecationReason : string) : FieldDef<'Val> =
        let resolver ctx' value' : GQLResult<'Res> Async = async {
            let! result = resolve ctx' value'
            return result |> ValueOption.map box |> ValueOption.toObj |> unbox |> Ok
        }
        Define.AsyncField (name, typedef, resolver, ?deprecationReason = deprecationReason)

    /// <summary>
    /// Creates field defined inside object type with asynchronously resolved value.
    /// </summary>
    /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
    /// <param name="typedef">GraphQL type definition of the current field's type.</param>
    /// <param name="description">Optional field description. Usefull for generating documentation.</param>
    /// <param name="resolve">Expression used to resolve value from defining object.</param>
    static member AsyncField(name : string, typedef : NullableDef<'Res>, description : string,
                             resolve : ResolveFieldContext -> 'Val -> Async<'Res voption>,
                             ?deprecationReason : string) : FieldDef<'Val> =
        let resolver ctx' value' : GQLResult<'Res> Async = async {
            let! result = resolve ctx' value'
            return result |> ValueOption.map box |> ValueOption.toObj |> unbox |> Ok
        }
        Define.AsyncField (name, typedef, description, resolver, ?deprecationReason = deprecationReason)

    /// <summary>
    /// Creates field defined inside object type with asynchronously resolved value.
    /// </summary>
    /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
    /// <param name="typedef">GraphQL type definition of the current field's type.</param>
    /// <param name="args">List of field arguments used to parametrize resolve expression output.</param>
    /// <param name="resolve">Expression used to resolve value from defining object.</param>
    /// <param name="deprecationReason">Deprecation reason.</param>
    static member AsyncField(name : string, typedef : NullableDef<'Res>, args : InputFieldDef list,
                             resolve : ResolveFieldContext -> 'Val -> Async<'Res voption>,
                             ?deprecationReason : string) : FieldDef<'Val> =
        let resolver ctx' value' : GQLResult<'Res> Async = async {
            let! result = resolve ctx' value'
            return result |> ValueOption.map box |> ValueOption.toObj |> unbox |> Ok
        }
        Define.AsyncField (name, typedef, args, resolver, ?deprecationReason = deprecationReason)

    /// <summary>
    /// Creates field defined inside object type with asynchronously resolved value.
    /// </summary>
    /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
    /// <param name="typedef">GraphQL type definition of the current field's type.</param>
    /// <param name="description">Optional field description. Usefull for generating documentation.</param>
    /// <param name="args">List of field arguments used to parametrize resolve expression output.</param>
    /// <param name="resolve">Expression used to resolve value from defining object.</param>
    /// <param name="deprecationReason">Deprecation reason.</param>
    static member AsyncField(name : string, typedef : NullableDef<'Res>, description : string,
                             args : InputFieldDef list,
                             resolve : ResolveFieldContext -> 'Val -> Async<'Res voption>,
                             ?deprecationReason : string) : FieldDef<'Val> =
        let resolver ctx' value' : GQLResult<'Res> Async = async {
            let! result = resolve ctx' value'
            return result |> ValueOption.map box |> ValueOption.toObj |> unbox |> Ok
        }
        Define.AsyncField (name, typedef, description, args, resolver, ?deprecationReason = deprecationReason)

// Async value

    /// <summary>
    /// Creates field defined inside object type with asynchronously resolved value.
    /// </summary>
    /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
    /// <param name="typedef">GraphQL type definition of the current field's type.</param>
    /// <param name="resolve">Expression used to resolve value from defining object.</param>
    static member AsyncField(name : string, typedef : OutputDef<'Res>,
                             resolve : ResolveFieldContext -> 'Val -> Async<'Res>,
                             ?deprecationReason : string) : FieldDef<'Val> =
        let resolver ctx' value' : GQLResult<'Res> Async = async {
            let! result = resolve ctx' value'
            return result |> Ok
        }
        Define.AsyncField (name, typedef, resolver, ?deprecationReason = deprecationReason)

    /// <summary>
    /// Creates field defined inside object type with asynchronously resolved value.
    /// </summary>
    /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
    /// <param name="typedef">GraphQL type definition of the current field's type.</param>
    /// <param name="description">Optional field description. Usefull for generating documentation.</param>
    /// <param name="resolve">Expression used to resolve value from defining object.</param>
    static member AsyncField(name : string, typedef : OutputDef<'Res>, description : string,
                             resolve : ResolveFieldContext -> 'Val -> Async<'Res>,
                             ?deprecationReason : string) : FieldDef<'Val> =
        let resolver ctx' value' : GQLResult<'Res> Async = async {
            let! result = resolve ctx' value'
            return result |> Ok
        }
        Define.AsyncField (name, typedef, description, resolver, ?deprecationReason = deprecationReason)

    /// <summary>
    /// Creates field defined inside object type with asynchronously resolved value.
    /// </summary>
    /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
    /// <param name="typedef">GraphQL type definition of the current field's type.</param>
    /// <param name="args">List of field arguments used to parametrize resolve expression output.</param>
    /// <param name="resolve">Expression used to resolve value from defining object.</param>
    /// <param name="deprecationReason">Deprecation reason.</param>
    static member AsyncField(name : string, typedef : OutputDef<'Res>, args : InputFieldDef list,
                             resolve : ResolveFieldContext -> 'Val -> Async<'Res>,
                             ?deprecationReason : string) : FieldDef<'Val> =
        let resolver ctx' value' : GQLResult<'Res> Async = async {
            let! result = resolve ctx' value'
            return result |> Ok
        }
        Define.AsyncField (name, typedef, args, resolver, ?deprecationReason = deprecationReason)

    /// <summary>
    /// Creates field defined inside object type with asynchronously resolved value.
    /// </summary>
    /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
    /// <param name="typedef">GraphQL type definition of the current field's type.</param>
    /// <param name="description">Optional field description. Usefull for generating documentation.</param>
    /// <param name="args">List of field arguments used to parametrize resolve expression output.</param>
    /// <param name="resolve">Expression used to resolve value from defining object.</param>
    /// <param name="deprecationReason">Deprecation reason.</param>
    static member AsyncField(name : string, typedef : OutputDef<'Res>, description : string,
                             args : InputFieldDef list,
                             resolve : ResolveFieldContext -> 'Val -> Async<'Res>,
                             ?deprecationReason : string) : FieldDef<'Val> =
        let resolver ctx' value' : GQLResult<'Res> Async = async {
            let! result = resolve ctx' value'
            return result |> Ok
        }
        Define.AsyncField (name, typedef, description, args, resolver, ?deprecationReason = deprecationReason)

    /// <summary>
    /// Creates a custom defined field using a custom field execution function.
    /// </summary>
    /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
    /// <param name="execField">Expression used to execute the field.</param>
    static member CustomField(name : string, [<ReflectedDefinition(true)>] execField : Expr<ExecuteField>) : FieldDef<'Val> =
        upcast { FieldDefinition.Name = name
                 Description = None
                 TypeDef = ObjType
                 Resolve = ResolveExpr(execField)
                 Args = [||]
                 DeprecationReason = None
                 Metadata = Metadata.Empty }

    /// <summary>
    /// Creates a subscription field inside object type.
    /// </summary>
    /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
    /// <param name="rootdef">GraphQL type definition of the root field's type.</param>
    /// <param name="outputdef">GraphQL type definition of the current field's type.</param>
    /// <param name="filter">A filter function which decides if the field should be published to clients or not, by returning it as Some or None.</param>
    static member SubscriptionField(name: string, rootdef: #OutputDef<'Root>, outputdef: #OutputDef<'Output>,
                                    [<ReflectedDefinition(true)>] filter: Expr<ResolveFieldContext -> 'Root -> 'Input -> 'Output option>): SubscriptionFieldDef<'Root, 'Input, 'Output> =
        upcast { Name = name
                 Description = None
                 RootTypeDef = rootdef
                 OutputTypeDef = outputdef
                 DeprecationReason = None
                 Args = [||]
                 Filter = Resolve.Filter(typeof<'Root>, typeof<'Input>, typeof<'Output>, filter)
                 Metadata = Metadata.Empty
                 TagsResolver = fun _ -> Seq.empty }

    /// <summary>
    /// Creates a subscription field inside object type.
    /// </summary>
    /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
    /// <param name="rootdef">GraphQL type definition of the root field's type.</param>
    /// <param name="outputdef">GraphQL type definition of the current field's type.</param>
    /// <param name="filter">A filter function which decides if the field should be published to clients or not, by returning it as Some or None.</param>
    /// <param name="tagsResolver">A function that resolves subscription tags, used to choose which filter functions will be used when publishing to subscribers.</param>
    static member SubscriptionField(name: string, rootdef: #OutputDef<'Root>, outputdef: #OutputDef<'Output>,
                                    [<ReflectedDefinition(true)>] filter: Expr<ResolveFieldContext -> 'Root -> 'Input -> 'Output option>,
                                    tagsResolver : TagsResolver): SubscriptionFieldDef<'Root, 'Input, 'Output> =
        upcast { Name = name
                 Description = None
                 RootTypeDef = rootdef
                 OutputTypeDef = outputdef
                 DeprecationReason = None
                 Args = [||]
                 Filter = Resolve.Filter(typeof<'Root>, typeof<'Input>, typeof<'Output>, filter)
                 Metadata = Metadata.Empty
                 TagsResolver = tagsResolver }

    /// <summary>
    /// Creates a subscription field inside object type.
    /// </summary>
    /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
    /// <param name="rootdef">GraphQL type definition of the root field's type.</param>
    /// <param name="outputdef">GraphQL type definition of the current field's type.</param>
    /// <param name="description">Optional field description. Usefull for generating documentation.</param>
    /// <param name="filter">A filter function which decides if the field should be published to clients or not, by returning it as Some or None.</param>
    static member SubscriptionField(name: string, rootdef: #OutputDef<'Root>, outputdef: #OutputDef<'Output>,
                                    description: string,
                                    [<ReflectedDefinition(true)>] filter: Expr<ResolveFieldContext -> 'Root -> 'Input -> 'Output option>): SubscriptionFieldDef<'Root, 'Input, 'Output> =
        upcast { Name = name
                 Description = Some description
                 RootTypeDef = rootdef
                 OutputTypeDef = outputdef
                 DeprecationReason = None
                 Args = [||]
                 Filter = Resolve.Filter(typeof<'Root>, typeof<'Input>, typeof<'Output>, filter)
                 Metadata = Metadata.Empty
                 TagsResolver = fun _ -> Seq.empty }

    /// <summary>
    /// Creates a subscription field inside object type.
    /// </summary>
    /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
    /// <param name="rootdef">GraphQL type definition of the root field's type.</param>
    /// <param name="outputdef">GraphQL type definition of the current field's type.</param>
    /// <param name="description">Optional field description. Usefull for generating documentation.</param>
    /// <param name="filter">A filter function which decides if the field should be published to clients or not, by returning it as Some or None.</param>
    /// <param name="tagsResolver">A function that resolves subscription tags, used to choose which filter functions will be used when publishing to subscribers.</param>
    static member SubscriptionField(name: string, rootdef: #OutputDef<'Root>, outputdef: #OutputDef<'Output>,
                                    description: string,
                                    [<ReflectedDefinition(true)>] filter: Expr<ResolveFieldContext -> 'Root -> 'Input -> 'Output option>,
                                    tagsResolver : TagsResolver): SubscriptionFieldDef<'Root, 'Input, 'Output> =
        upcast { Name = name
                 Description = Some description
                 RootTypeDef = rootdef
                 OutputTypeDef = outputdef
                 DeprecationReason = None
                 Args = [||]
                 Filter = Resolve.Filter(typeof<'Root>, typeof<'Input>, typeof<'Output>, filter)
                 Metadata = Metadata.Empty
                 TagsResolver = tagsResolver }

    /// <summary>
    /// Creates a subscription field inside object type.
    /// </summary>
    /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
    /// <param name="rootdef">GraphQL type definition of the root field's type.</param>
    /// <param name="outputdef">GraphQL type definition of the current field's type.</param>
    /// <param name="description">Optional field description. Usefull for generating documentation.</param>
    /// <param name="args">List of field arguments used to parametrize resolve expression output.</param>
    /// <param name="filter">A filter function which decides if the field should be published to clients or not, by returning it as Some or None.</param>
    static member SubscriptionField(name: string, rootdef: #OutputDef<'Root>, outputdef: #OutputDef<'Output>,
                                    description: string,
                                    args: InputFieldDef list,
                                    [<ReflectedDefinition(true)>] filter: Expr<ResolveFieldContext -> 'Root -> 'Input -> 'Output option>): SubscriptionFieldDef<'Root, 'Input, 'Output> =
        upcast { Name = name
                 Description = Some description
                 RootTypeDef = rootdef
                 OutputTypeDef = outputdef
                 DeprecationReason = None
                 Args = args |> List.toArray
                 Filter = Resolve.Filter(typeof<'Root>, typeof<'Input>, typeof<'Output>, filter)
                 Metadata = Metadata.Empty
                 TagsResolver = fun _ -> Seq.empty }

    /// <summary>
    /// Creates a subscription field inside object type.
    /// </summary>
    /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
    /// <param name="rootdef">GraphQL type definition of the root field's type.</param>
    /// <param name="outputdef">GraphQL type definition of the current field's type.</param>
    /// <param name="description">Optional field description. Usefull for generating documentation.</param>
    /// <param name="args">List of field arguments used to parametrize resolve expression output.</param>
    /// <param name="filter">A filter function which decides if the field should be published to clients or not, by returning it as Some or None.</param>
    /// <param name="tagsResolver">A function that resolves subscription tags, used to choose which filter functions will be used when publishing to subscribers.</param>
    static member SubscriptionField(name: string, rootdef: #OutputDef<'Root>, outputdef: #OutputDef<'Output>,
                                    description: string,
                                    args: InputFieldDef list,
                                    [<ReflectedDefinition(true)>] filter: Expr<ResolveFieldContext -> 'Root -> 'Input -> 'Output option>,
                                    tagsResolver : TagsResolver): SubscriptionFieldDef<'Root, 'Input, 'Output> =
        upcast { Name = name
                 Description = Some description
                 RootTypeDef = rootdef
                 OutputTypeDef = outputdef
                 DeprecationReason = None
                 Args = args |> List.toArray
                 Filter = Resolve.Filter(typeof<'Root>, typeof<'Input>, typeof<'Output>, filter)
                 Metadata = Metadata.Empty
                 TagsResolver = tagsResolver }

    /// <summary>
    /// Creates a subscription field inside object type. Field is marked as deprecated.
    /// </summary>
    /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
    /// <param name="rootdef">GraphQL type definition of the root field's type.</param>
    /// <param name="outputdef">GraphQL type definition of the current field's type.</param>
    /// <param name="description">Optional field description. Usefull for generating documentation.</param>
    /// <param name="args">List of field arguments used to parametrize resolve expression output.</param>
    /// <param name="filter">A filter function which decides if the field should be published to clients or not, by returning it as Some or None.</param>
    /// <param name="deprecationReason">Deprecation reason.</param>
    static member SubscriptionField(name: string, rootdef: #OutputDef<'Root>, outputdef: #OutputDef<'Output>,
                                    description: string,
                                    args: InputFieldDef list,
                                    [<ReflectedDefinition(true)>] filter: Expr<ResolveFieldContext -> 'Root -> 'Input -> 'Output option>,
                                    deprecationReason : string): SubscriptionFieldDef<'Root, 'Input, 'Output> =
        upcast { Name = name
                 Description = Some description
                 RootTypeDef = rootdef
                 OutputTypeDef = outputdef
                 DeprecationReason = Some deprecationReason
                 Args = args |> List.toArray
                 Filter = Resolve.Filter(typeof<'Root>, typeof<'Input>, typeof<'Output>, filter)
                 Metadata = Metadata.Empty
                 TagsResolver = fun _ -> Seq.empty }

    /// <summary>
    /// Creates a subscription field inside object type.
    /// </summary>
    /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
    /// <param name="rootdef">GraphQL type definition of the root field's type.</param>
    /// <param name="outputdef">GraphQL type definition of the current field's type.</param>
    /// <param name="description">Optional field description. Usefull for generating documentation.</param>
    /// <param name="args">List of field arguments used to parametrize resolve expression output.</param>
    /// <param name="filter">A filter function which decides if the field should be published to clients or not, by returning it as Some or None.</param>
    /// <param name="tagsResolver">A function that resolves subscription tags, used to choose which filter functions will be used when publishing to subscribers.</param>
    /// <param name="deprecationReason">Deprecation reason.</param>
    static member SubscriptionField(name: string, rootdef: #OutputDef<'Root>, outputdef: #OutputDef<'Output>,
                                    description: string,
                                    args: InputFieldDef list,
                                    [<ReflectedDefinition(true)>] filter: Expr<ResolveFieldContext -> 'Root -> 'Input -> 'Output option>,
                                    tagsResolver : TagsResolver,
                                    deprecationReason : string): SubscriptionFieldDef<'Root, 'Input, 'Output> =
        upcast { Name = name
                 Description = Some description
                 RootTypeDef = rootdef
                 OutputTypeDef = outputdef
                 DeprecationReason = Some deprecationReason
                 Args = args |> List.toArray
                 Filter = Resolve.Filter(typeof<'Root>, typeof<'Input>, typeof<'Output>, filter)
                 Metadata = Metadata.Empty
                 TagsResolver = tagsResolver }

    /// <summary>
    /// Creates a subscription field inside object type, with asynchronously resolved value.
    /// </summary>
    /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
    /// <param name="rootdef">GraphQL type definition of the root field's type.</param>
    /// <param name="outputdef">GraphQL type definition of the current field's type.</param>
    /// <param name="filter">A filter function which decides if the field should be published to clients or not, by returning it as Some or None.</param>
    static member AsyncSubscriptionField(name: string, rootdef: #OutputDef<'Root>, outputdef: #OutputDef<'Output>,
                                         [<ReflectedDefinition(true)>] filter: Expr<ResolveFieldContext -> 'Root -> 'Input -> Async<'Output option>>): SubscriptionFieldDef<'Root, 'Input, 'Output> =
        upcast { Name = name
                 Description = None
                 RootTypeDef = rootdef
                 OutputTypeDef = outputdef
                 DeprecationReason = None
                 Args = [||]
                 Filter = Resolve.AsyncFilter(typeof<'Root>, typeof<'Input>, typeof<'Output>, filter)
                 Metadata = Metadata.Empty
                 TagsResolver = fun _ -> Seq.empty }

    /// <summary>
    /// Creates a subscription field inside object type, with asynchronously resolved value.
    /// </summary>
    /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
    /// <param name="rootdef">GraphQL type definition of the root field's type.</param>
    /// <param name="outputdef">GraphQL type definition of the current field's type.</param>
    /// <param name="filter">A filter function which decides if the field should be published to clients or not, by returning it as Some or None.</param>
    /// <param name="tagsResolver">A function that resolves subscription tags, used to choose which filter functions will be used when publishing to subscribers.</param>
    static member AsyncSubscriptionField(name: string, rootdef: #OutputDef<'Root>, outputdef: #OutputDef<'Output>,
                                         [<ReflectedDefinition(true)>] filter: Expr<ResolveFieldContext -> 'Root -> 'Input -> Async<'Output option>>,
                                         tagsResolver : TagsResolver): SubscriptionFieldDef<'Root, 'Input, 'Output> =
        upcast { Name = name
                 Description = None
                 RootTypeDef = rootdef
                 OutputTypeDef = outputdef
                 DeprecationReason = None
                 Args = [||]
                 Filter = Resolve.AsyncFilter(typeof<'Root>, typeof<'Input>, typeof<'Output>, filter)
                 Metadata = Metadata.Empty
                 TagsResolver = tagsResolver }

    /// <summary>
    /// Creates a subscription field inside object type, with asynchronously resolved value.
    /// </summary>
    /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
    /// <param name="rootdef">GraphQL type definition of the root field's type.</param>
    /// <param name="outputdef">GraphQL type definition of the current field's type.</param>
    /// <param name="description">Optional field description. Usefull for generating documentation.</param>
    /// <param name="filter">A filter function which decides if the field should be published to clients or not, by returning it as Some or None.</param>
    static member AsyncSubscriptionField(name: string, rootdef: #OutputDef<'Root>, outputdef: #OutputDef<'Output>,
                                         description: string,
                                         [<ReflectedDefinition(true)>] filter: Expr<ResolveFieldContext -> 'Root -> 'Input -> Async<'Output option>>): SubscriptionFieldDef<'Root, 'Input, 'Output> =
        upcast { Name = name
                 Description = Some description
                 RootTypeDef = rootdef
                 OutputTypeDef = outputdef
                 DeprecationReason = None
                 Args = [||]
                 Filter = Resolve.AsyncFilter(typeof<'Root>, typeof<'Input>, typeof<'Output>, filter)
                 Metadata = Metadata.Empty
                 TagsResolver = fun _ -> Seq.empty }

    /// <summary>
    /// Creates a subscription field inside object type, with asynchronously resolved value.
    /// </summary>
    /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
    /// <param name="rootdef">GraphQL type definition of the root field's type.</param>
    /// <param name="outputdef">GraphQL type definition of the current field's type.</param>
    /// <param name="description">Optional field description. Usefull for generating documentation.</param>
    /// <param name="filter">A filter function which decides if the field should be published to clients or not, by returning it as Some or None.</param>
    /// <param name="tagsResolver">A function that resolves subscription tags, used to choose which filter functions will be used when publishing to subscribers.</param>
    static member AsyncSubscriptionField(name: string, rootdef: #OutputDef<'Root>, outputdef: #OutputDef<'Output>,
                                         description: string,
                                         [<ReflectedDefinition(true)>] filter: Expr<ResolveFieldContext -> 'Root -> 'Input -> Async<'Output option>>,
                                         tagsResolver : TagsResolver): SubscriptionFieldDef<'Root, 'Input, 'Output> =
        upcast { Name = name
                 Description = Some description
                 RootTypeDef = rootdef
                 OutputTypeDef = outputdef
                 DeprecationReason = None
                 Args = [||]
                 Filter = Resolve.AsyncFilter(typeof<'Root>, typeof<'Input>, typeof<'Output>, filter)
                 Metadata = Metadata.Empty
                 TagsResolver = tagsResolver }

    /// <summary>
    /// Creates a subscription field inside object type, with asynchronously resolved value.
    /// </summary>
    /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
    /// <param name="rootdef">GraphQL type definition of the root field's type.</param>
    /// <param name="outputdef">GraphQL type definition of the current field's type.</param>
    /// <param name="description">Optional field description. Usefull for generating documentation.</param>
    /// <param name="args">List of field arguments used to parametrize resolve expression output.</param>
    /// <param name="filter">A filter function which decides if the field should be published to clients or not, by returning it as Some or None.</param>
    static member AsyncSubscriptionField(name: string, rootdef: #OutputDef<'Root>, outputdef: #OutputDef<'Output>,
                                         description: string,
                                         args: InputFieldDef list,
                                         [<ReflectedDefinition(true)>] filter: Expr<ResolveFieldContext -> 'Root -> 'Input -> Async<'Output option>>): SubscriptionFieldDef<'Root, 'Input, 'Output> =
        upcast { Name = name
                 Description = Some description
                 RootTypeDef = rootdef
                 OutputTypeDef = outputdef
                 DeprecationReason = None
                 Args = args |> List.toArray
                 Filter = Resolve.AsyncFilter(typeof<'Root>, typeof<'Input>, typeof<'Output>, filter)
                 Metadata = Metadata.Empty
                 TagsResolver = fun _ -> Seq.empty }

    /// <summary>
    /// Creates a subscription field inside object type, with asynchronously resolved value.
    /// </summary>
    /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
    /// <param name="rootdef">GraphQL type definition of the root field's type.</param>
    /// <param name="outputdef">GraphQL type definition of the current field's type.</param>
    /// <param name="description">Optional field description. Usefull for generating documentation.</param>
    /// <param name="args">List of field arguments used to parametrize resolve expression output.</param>
    /// <param name="filter">A filter function which decides if the field should be published to clients or not, by returning it as Some or None.</param>
    /// <param name="tagsResolver">A function that resolves subscription tags, used to choose which filter functions will be used when publishing to subscribers.</param>
    static member AsyncSubscriptionField(name: string, rootdef: #OutputDef<'Root>, outputdef: #OutputDef<'Output>,
                                         description: string,
                                         args: InputFieldDef list,
                                         [<ReflectedDefinition(true)>] filter: Expr<ResolveFieldContext -> 'Root -> 'Input -> Async<'Output option>>,
                                         tagsResolver : TagsResolver): SubscriptionFieldDef<'Root, 'Input, 'Output> =
        upcast { Name = name
                 Description = Some description
                 RootTypeDef = rootdef
                 OutputTypeDef = outputdef
                 DeprecationReason = None
                 Args = args |> List.toArray
                 Filter = Resolve.AsyncFilter(typeof<'Root>, typeof<'Input>, typeof<'Output>, filter)
                 Metadata = Metadata.Empty
                 TagsResolver = tagsResolver }

    /// <summary>
    /// Creates a subscription field inside object type, with asynchronously resolved value. Field is marked as deprecated.
    /// </summary>
    /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
    /// <param name="rootdef">GraphQL type definition of the root field's type.</param>
    /// <param name="outputdef">GraphQL type definition of the current field's type.</param>
    /// <param name="description">Optional field description. Usefull for generating documentation.</param>
    /// <param name="args">List of field arguments used to parametrize resolve expression output.</param>
    /// <param name="filter">A filter function which decides if the field should be published to clients or not, by returning it as Some or None.</param>
    /// <param name="deprecationReason">Deprecation reason.</param>
    static member AsyncSubscriptionField(name: string, rootdef: #OutputDef<'Root>, outputdef: #OutputDef<'Output>,
                                         description: string,
                                         args: InputFieldDef list,
                                         [<ReflectedDefinition(true)>] filter: Expr<ResolveFieldContext -> 'Root -> 'Input -> Async<'Output option>>,
                                         deprecationReason : string): SubscriptionFieldDef<'Root, 'Input, 'Output> =
        upcast { Name = name
                 Description = Some description
                 RootTypeDef = rootdef
                 OutputTypeDef = outputdef
                 DeprecationReason = Some deprecationReason
                 Args = args |> List.toArray
                 Filter = Resolve.AsyncFilter(typeof<'Root>, typeof<'Input>, typeof<'Output>, filter)
                 Metadata = Metadata.Empty
                 TagsResolver = fun _ -> Seq.empty }

    /// <summary>
    /// Creates a subscription field inside object type, with asynchronously resolved value.
    /// </summary>
    /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
    /// <param name="rootdef">GraphQL type definition of the root field's type.</param>
    /// <param name="outputdef">GraphQL type definition of the current field's type.</param>
    /// <param name="description">Optional field description. Usefull for generating documentation.</param>
    /// <param name="args">List of field arguments used to parametrize resolve expression output.</param>
    /// <param name="filter">A filter function which decides if the field should be published to clients or not, by returning it as Some or None.</param>
    /// <param name="tagsResolver">A function that resolves subscription tags, used to choose which filter functions will be used when publishing to subscribers.</param>
    /// <param name="deprecationReason">Deprecation reason.</param>
    static member AsyncSubscriptionField(name: string, rootdef: #OutputDef<'Root>, outputdef: #OutputDef<'Output>,
                                         description: string,
                                         args: InputFieldDef list,
                                         [<ReflectedDefinition(true)>] filter: Expr<ResolveFieldContext -> 'Root -> 'Input -> Async<'Output option>>,
                                         tagsResolver : TagsResolver,
                                         deprecationReason : string): SubscriptionFieldDef<'Root, 'Input, 'Output> =
        upcast { Name = name
                 Description = Some description
                 RootTypeDef = rootdef
                 OutputTypeDef = outputdef
                 DeprecationReason = Some deprecationReason
                 Args = args |> List.toArray
                 Filter = Resolve.AsyncFilter(typeof<'Root>, typeof<'Input>, typeof<'Output>, filter)
                 Metadata = Metadata.Empty
                 TagsResolver = tagsResolver }


    /// <summary>
    /// Creates an input field. Input fields are used like ordinary fileds in case of <see cref="InputObject"/>s,
    /// and can be used to define arguments to objects and interfaces fields.
    /// </summary>
    /// <param name="name">
    /// Field name. Must be unique in scope of the defining input object or withing field's argument list.
    /// </param>
    /// <param name="typedef">GraphQL type definition of the current input type</param>
    /// <param name="defaultValue">If defined, this value will be used when no matching input has been provided by the requester.</param>
    /// <param name="description">Optional input description. Usefull for generating documentation.</param>
    static member Input(name : string, typedef : #InputDef<'In>, ?defaultValue : 'In, ?description : string) : InputFieldDef =
        upcast ({ Name = name
                  Description = description
                  TypeDef = typedef
                  DefaultValue = defaultValue
                  ExecuteInput = Unchecked.defaultof<ExecuteInput> } : InputFieldDefinition<'In>)

    /// <summary>
    /// Creates a custom GraphQL interface type. It's needs to be implemented by object types and should not be used alone.
    /// </summary>
    /// <param name="name">Type name. Must be unique in scope of the current schema.</param>
    /// <param name="fieldsFn">
    /// Function which generates a list of fields defined by the current interface. Usefull, when object defines recursive dependencies.
    /// </param>
    /// <param name="description">Optional input description. Usefull for generating documentation.</param>
    /// <param name="resolveType">Optional function used to resolve actual Object definition of the .NET object provided as an input.</param>
    static member Interface(name : string, fieldsFn : unit -> FieldDef<'Val> list, ?description : string,
                            ?resolveType : obj -> ObjectDef) : InterfaceDef<'Val> =
        upcast { InterfaceDefinition.Name = name
                 Description = description
                 FieldsFn = fun () -> fieldsFn() |> List.toArray
                 ResolveType = resolveType }

    /// <summary>
    /// Creates a custom GraphQL interface type. It's needs to be implemented by object types and should not be used alone.
    /// </summary>
    /// <param name="name">Type name. Must be unique in scope of the current schema.</param>
    /// <param name="fields">List of fields defined by the current interface.</param>
    /// <param name="description">Optional input description. Usefull for generating documentation.</param>
    /// <param name="resolveType">Optional function used to resolve actual Object definition of the .NET object provided as an input.</param>
    static member Interface(name : string, fields : FieldDef<'Val> list, ?description : string,
                            ?resolveType : obj -> ObjectDef) : InterfaceDef<'Val> =
        upcast { InterfaceDefinition.Name = name
                 Description = description
                 FieldsFn = fun () -> fields |> List.toArray
                 ResolveType = resolveType }

    /// <summary>
    /// Creates a custom GraphQL union type, materialized as one of the types defined. It can be used as interface/object type field.
    /// In order to work with F# discriminated unions, <paramref name="resolveValue"/> function may be used to unwrap objects
    /// nested as discriminated union cases.
    /// </summary>
    /// <param name="name">Type name. Must be unique in scope of the current schema.</param>
    /// <param name="options">Interfaces or objects used as union cases.</param>
    /// <param name="resolveValue">Given F# discriminated union as input, returns .NET object valid with one of the defined GraphQL union cases.</param>
    /// <param name="resolveType">Resolves an Object definition of one of possible types, give input object.</param>
    /// <param name="description">Optional union description. Usefull for generating documentation.</param>
    static member Union(name : string, options : ObjectDef list, resolveValue : 'In -> 'Out,
                        ?resolveType : 'In -> ObjectDef, ?description : string) : UnionDef<'In> =
        upcast { UnionDefinition.Name = name
                 Description = description
                 Options = options |> List.toArray
                 ResolveType = resolveType
                 ResolveValue = resolveValue }
