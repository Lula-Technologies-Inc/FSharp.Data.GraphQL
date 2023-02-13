namespace FSharp.Data.GraphQL.IntegrationTests.Server

open System.Collections.Immutable
open System.Text.Json
open System.Text.Json.Nodes
open System.Text.Json.Serialization
open Microsoft.FSharp.Reflection

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Types.Patterns

type private GQLEditableRequestContent =
    { Query : string
      OperationName : string voption
      Variables : JsonObject voption }

[<AutoOpen>]
module JsonNodeExtensions =

    open System.Buffers

    type JsonNode with

        static member Create object =
            let bufferWriter = new ArrayBufferWriter<byte>();
            use writer = new Utf8JsonWriter(bufferWriter)
            JsonSerializer.Serialize(writer, object, Json.serializerOptions)
            JsonSerializer.Deserialize<JsonNode>(bufferWriter.WrittenSpan, Json.serializerOptions)

        member node.AsJsonElement() =
            let bufferWriter = new ArrayBufferWriter<byte>()
            use writer = new Utf8JsonWriter(bufferWriter)
            node.WriteTo (writer, Json.serializerOptions)
            let bytes = bufferWriter.WrittenSpan
            let mutable reader = new Utf8JsonReader(bytes)
            JsonDocument.ParseValue(&reader).RootElement

[<Sealed>]
type GraphQLQueryConverter<'a>(executor : Executor<'a>, replacements: Map<string, obj>, ?meta : Metadata) =
    inherit JsonConverter<GraphQLQuery>()

    /// Active pattern to match GraphQL type defintion with nullable / optional types.
    let (|Nullable|_|) (tdef : TypeDef) =
        match tdef with
        | :? NullableDef as x -> Some x.OfType
        | _ -> None

    override __.CanConvert(t) = t = typeof<GraphQLQuery>

    override __.Write(_, _, _) = raise <| System.NotSupportedException()

    override __.Read(reader, _, options) =

        let request = JsonSerializer.Deserialize<GQLEditableRequestContent>(&reader, options)
        let plan =
            let query = request.Query
            match meta with
            | Some meta -> executor.CreateExecutionPlan(query, meta = meta)
            | None -> executor.CreateExecutionPlan(query)
        let varDefs = plan.Variables
        match varDefs with
        | [] -> { ExecutionPlan = plan; Variables = ImmutableDictionary.Empty }
        | vs ->
            // For multipart requests, we need to replace some variables
            let vars = request.Variables.Value
            // TODO: Implement JSON path
            Map.iter (fun path rep ->
                          vars.Remove path |> ignore
                          vars.Add(path, JsonNode.Create rep))
                     replacements
            //Map.iter(fun path rep -> vars.SelectToken(path).Replace(JObject.FromObject(rep))) replacements
            let variables =
                vs
                |> List.fold (fun (acc: ImmutableDictionary<string, JsonElement>.Builder) (vdef: VarDef) ->
                    match vars.TryGetPropertyValue vdef.Name with
                    | true, jsonNode ->
                        let jsonElement = jsonNode.AsJsonElement()
                        acc.Add (vdef.Name, jsonElement)
                    | false, _  ->
                        match vdef.DefaultValue, vdef.TypeDef with
                        | Some _, _ -> ()
                        | _, Nullable _ -> ()
                        | None, _ -> failwithf "Variable %s has no default value and is missing!" vdef.Name
                    acc)
                    (ImmutableDictionary.CreateBuilder<string, JsonElement>())
            { ExecutionPlan = plan; Variables = variables.ToImmutable() }

open System
open System.Collections.Generic

[<AutoOpen>]
module private GraphQLSubscriptionFields =
    let [<Literal>] FIELD_Type = "type"
    let [<Literal>] FIELD_Id = "id"
    let [<Literal>] FIELD_Payload = "payload"
    let [<Literal>] FIELD_Error = "error"
