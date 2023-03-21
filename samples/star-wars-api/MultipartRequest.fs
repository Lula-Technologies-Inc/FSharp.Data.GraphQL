namespace FSharp.Data.GraphQL.Samples.StarWarsApi

open System
open System.Collections
open System.Collections.Generic
open System.Collections.Immutable
open System.Text.Json
open System.Text.Json.Nodes
open System.Text.Json.Serialization
open Microsoft.AspNetCore.WebUtilities

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Uploading


[<Struct>]
type GraphQLMultipartSection =
    | FormSection of Form : FormMultipartSection
    | FileSection of File : FileMultipartSection

    static member FromSection(section : MultipartSection) =
        match section with
        | null -> ValueNone
        | _ ->
            match section.AsFormDataSection() with
            | null ->
                match section.AsFileSection() with
                | null -> ValueNone
                | x -> ValueSome (FileSection x)
            | x -> ValueSome (FormSection x)

    member x.Name =
        match x with
        | FormSection x -> x.Name
        | FileSection x -> x.Name


/// A GraphQL operation request.
// Use GQLRequestContent from HttpHandler (need to move it so it's internal, and include it here)
//type Operation =
//      /// Contains the query used in this operation.
//    { Query : string
//      /// Contains variables used by this operation.
//      Variables : Map<string, obj> option }

/// <summary> A GrahpQL request using multipart request specification. </summary>
/// <remarks> For more information, see https://github.com/jaydenseric/graphql-multipart-request-spec. </remarks>
type MultipartRequest =
      /// Contains the list of operations of this request.
      /// If the request is not batched, then the single operation will be inside this list as a singleton.
    { Operations : GQLRequestContent list }

/// Contains tools for working with GraphQL multipart requests.
module MultipartRequest =
    let private parseOperations (operations: GQLRequestContent list) (map : IDictionary<string, string>) (files : IDictionary<string, File>) : GQLRequestContent list =
        let mapOperation (operationIndex : int option) (operation : GQLRequestContent) =
            let findFile (varName : string) (varValue : JsonElement) =
                let tryPickMultipleFilesFromMap (length : int) (varName : string) =
                    Seq.init length (fun ix ->
                        match map.TryGetValue(sprintf "%s.%i" varName ix) with
                        | (true, v) -> Some v
                        | _ -> None)
                    |> Seq.map (fun key ->
                        key |> Option.map (fun key ->
                            match files.TryGetValue(key) with
                            | (true, v) -> Some v
                            | _ -> None)
                        |> Option.flatten)
                    |> List.ofSeq
                let pickMultipleFilesFromMap (length : int) (varName : string) =
                    Seq.init length (fun ix -> map.[sprintf "%s.%i" varName ix])
                    |> Seq.map (fun key -> files.[key])
                    |> List.ofSeq
                let tryPickSingleFileFromMap varName =
                    let found = map |> Seq.choose (fun kvp -> if kvp.Key = varName then Some files.[kvp.Value] else None) |> List.ofSeq
                    match found with
                    | [x] -> Some x
                    | _ -> None
                let pickSingleFileFromMap varName =
                    map
                    |> Seq.choose (fun kvp -> if kvp.Key = varName then Some files.[kvp.Value] else None)
                    |> Seq.exactlyOne
                let pickFileRequestFromMap (request : UploadRequest) varName =
                    { UploadRequest.Single = pickSingleFileFromMap (sprintf "%s.single" varName)
                      Multiple = pickMultipleFilesFromMap request.Multiple.Length (sprintf "%s.multiple" varName)
                      NullableMultiple = request.NullableMultiple |> Option.map (fun x -> pickMultipleFilesFromMap x.Length (sprintf "%s.nullableMultiple" varName))
                      NullableMultipleNullable = request.NullableMultipleNullable |> Option.map (fun x -> tryPickMultipleFilesFromMap x.Length (sprintf "%s.nullableMultipleNullable" varName)) }
                let rec isUpload (t : InputType) =
                    match t with
                    | NamedType tname -> tname = "Upload" || tname = "UploadRequest"
                    | ListType t | NonNullType t -> isUpload t
                let ast = Parser.parse operation.Query
                let vardefs =
                    ast.Definitions
                    |> List.choose (function OperationDefinition def -> Some def.VariableDefinitions | _ -> None)
                    |> List.collect id
                let vardef = vardefs |> List.find (fun x -> x.VariableName = varName)
                if not (isUpload vardef.Type)
                then varValue
                else
                    match varValue with
                    | _ when varValue.ValueKind = JsonValueKind.Object ->
                        let request = JsonSerializer.Deserialize<UploadRequest>(varValue)
                        //let request = jreq.Deserialize<UploadRequest>(jsonSerializer)
                        let varName =
                            match operationIndex with
                            | Some operationIndex -> sprintf "%i.variables.%s" operationIndex varName
                            | None -> sprintf "variables.%s" varName
                        pickFileRequestFromMap request varName |> box
                    | _ when varValue.ValueKind = JsonValueKind.Array ->
                        varValue.EnumerateArray()
                        |> Seq.cast<obj>
                        |> Seq.mapi (fun valueIndex _ ->
                            let varName =
                                match operationIndex with
                                | Some operationIndex -> sprintf "%i.variables.%s.%i" operationIndex varName valueIndex
                                | None -> sprintf "variables.%s.%i" varName valueIndex
                            tryPickSingleFileFromMap varName |> Option.map box |> Option.toObj)
                        |> box
                    | _ ->
                        let varName =
                            match operationIndex with
                            | Some operationIndex -> sprintf "%i.variables.%s" operationIndex varName
                            | None -> sprintf "variables.%s" varName
                        tryPickSingleFileFromMap varName |> Option.map box |> Option.toObj
            let varsMaybe = operation.Variables |> Skippable.toOption
            let varsMaybeWithFiles = varsMaybe |> Option.map (fun vars ->
                    vars |> Seq.map (fun k -> (k.Key, (findFile k.Key k.Value))) |> Map.ofSeq |> ImmutableDictionary.ToImmutableDictionary
                )
            { operation with Variables = varsMaybeWithFiles |> Skippable.ofOption }
        match operations with
        | [ operation ] -> [ mapOperation None operation ]
        | operations -> operations |> List.mapi (fun ix operation -> mapOperation (Some ix) operation)

    /// Reads a GraphQL multipart request from a MultipartReader.
    let read cancellationToken (reader : MultipartReader) : Threading.Tasks.Task<MultipartRequest> =
        task {
            let mutable section : GraphQLMultipartSection voption = ValueNone
            let readNextSection () =
                task {
                    let! next = reader.ReadNextSectionAsync cancellationToken
                    section <- GraphQLMultipartSection.FromSection(next)
                }
            let mutable operations : string = null
            let mutable map : IDictionary<string, string> = null
            let files = Dictionary<string, File>()
            do! readNextSection ()
            while not section.IsNone do
                match section.Value with
                | FormSection section ->
                    let! value = section.GetValueAsync()
                    match section.Name with
                    | "operations" ->
                        operations <- value
                    | "map" ->
                        map <- JsonSerializer.Deserialize<Map<string, string list>>(value)
                               |> Seq.map (fun kvp -> kvp.Value.Head, kvp.Key)
                               |> Map.ofSeq
                    | _ -> failwithf "Error reading multipart request. Unexpected section name \"%s\"." section.Name
                | FileSection section ->
                    let stream = new System.IO.MemoryStream(4096)
                    do! section.FileStream.CopyToAsync(stream, cancellationToken) |> Async.AwaitTask
                    stream.Position <- 0L
                    let value = { File.Name = section.FileName; ContentType = section.Section.ContentType; Content = stream }
                    files.Add(section.Name, value)
                do! readNextSection ()
            let operations =
                match JToken.Parse(operations) with
                | :? JArray as ops -> ops.ToObject<GQLRequestContent list>(jsonSerializer)
                | :? JObject as op -> [ op.ToObject<GQLRequestContent>(jsonSerializer) ]
                | _ -> failwith "Unexpected operations value."
            return { Operations = parseOperations operations map files }
        }
