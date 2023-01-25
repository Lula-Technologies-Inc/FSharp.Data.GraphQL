[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module public FSharp.Data.GraphQL.Json

open System.Text.Json
open System.Text.Json.Serialization

let [<Literal>] UnionTag = "kind"

#nowarn "0058"
let configureSerializerOptions (additionalConverters: JsonConverter seq) (options : JsonSerializerOptions) =
    options.PropertyNamingPolicy <- JsonNamingPolicy.CamelCase
    options.PropertyNameCaseInsensitive <- true
    //options.DefaultIgnoreCondition <- JsonIgnoreCondition.WhenWritingNull
    let converters = options.Converters
    converters.Add (JsonStringEnumConverter ())
    additionalConverters |> Seq.iter converters.Add
    converters.Add (
        JsonFSharpConverter(
            JsonUnionEncoding.InternalTag ||| JsonUnionEncoding.AllowUnorderedTag ||| JsonUnionEncoding.NamedFields
            ||| JsonUnionEncoding.UnwrapSingleCaseUnions
            ||| JsonUnionEncoding.UnwrapRecordCases
            ||| JsonUnionEncoding.UnwrapOption, UnionTag))
    options

let getSerializerOptions (additionalConverters: JsonConverter seq) =
    // TODO GBirkel: Needed to make Tests.IntrospectionTests.Introspection pass, but makes other tests fail?
    // https://github.com/Tarmil/FSharp.SystemTextJson/blob/master/docs/Customizing.md#skippable-option-fields
    //let options =
    //        JsonFSharpOptions.Default()
    //            .WithSkippableOptionFields()
    //            .ToJsonSerializerOptions()
    //configureSerializerOptions additionalConverters options
    JsonSerializerOptions () |> configureSerializerOptions additionalConverters

let public serializerOptions = getSerializerOptions Seq.empty
let public configureOptions  = configureSerializerOptions
