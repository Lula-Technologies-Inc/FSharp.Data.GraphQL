namespace FSharp.Data.GraphQL.Samples.StarWarsApi

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.IO
open System.Text.Json
open System.Text.Json.Serialization
open System.Threading.Tasks
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Http.Json
open Microsoft.AspNetCore.WebUtilities
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Logging
open Microsoft.Extensions.Options

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Uploading
open Giraffe

type HttpHandler = HttpFunc -> HttpContext -> HttpFuncResult


module HttpHandlers =

    let rec private moduleType = getModuleType <@ moduleType @>

    let ofTaskIResult ctx (taskRes : Task<IResult>) : HttpFuncResult =
        task {
            let! res = taskRes
            do! res.ExecuteAsync (ctx)
            return Some ctx
        }

    /// Set CORS to allow external servers (React samples) to call this API
    let setCorsHeaders : HttpHandler =
        setHttpHeader "Access-Control-Allow-Origin" "*"
        >=> setHttpHeader "Access-Control-Allow-Headers" "content-type"


    let private graphQL (next : HttpFunc) (ctx : HttpContext) =

        let logger = ctx.RequestServices.CreateLogger moduleType

        let jsonSerializerOptions =
            ctx.RequestServices
                .GetRequiredService<IOptions<JsonOptions>>()
                .Value.SerializerOptions

        let serializeWithOptions data = JsonSerializer.Serialize (data, jsonSerializerOptions)
        let request = ctx.Request

        // TODO: validate the result
        /// Resolve response type and wrap it into an appropriate object
        let toResponse { DocumentId = documentId; Content = content; Metadata = metadata } =
            match content with
            | Direct (data, errs) ->
                logger.LogInformation (
                    $"Produced direct GraphQL response with documentId = '{{documentId}}' and metadata:{Environment.NewLine}{{metadata}}",
                    documentId,
                    metadata
                )

                if logger.IsEnabled LogLevel.Trace then
                    logger.LogTrace ($"GraphQL response data:{Environment.NewLine}{{data}}", serializeWithOptions data)

                GQLResponse.Direct (documentId, data, errs)

            | Deferred (data, errs, deferred) ->
                logger.LogInformation (
                    $"Produced deferred GraphQL response with documentId = '{{documentId}}' and metadata:{Environment.NewLine}{{metadata}}",
                    documentId,
                    metadata
                )

                if errs.Length > 0 then
                    logger.LogTrace ($"{{number}} errors:{Environment.NewLine}{{errs}}", errs.Length, errs)

                if logger.IsEnabled LogLevel.Information then
                    deferred
                    |> Observable.add (function
                        | DeferredResult (data, path) ->
                            logger.LogInformation (
                                "Produced GraphQL deferred result for path: {path}",
                                path |> Seq.map string |> Seq.toArray |> Path.Join
                            )

                            if logger.IsEnabled LogLevel.Trace then
                                logger.LogTrace ($"GraphQL deferred data:{Environment.NewLine}{{data}}", serializeWithOptions data)
                        | DeferredErrors (null, errors, path) ->
                            logger.LogInformation (
                                "Produced GraphQL deferred errors for path: {path}",
                                path |> Seq.map string |> Seq.toArray |> Path.Join
                            )

                            logger.LogTrace ($"GraphQL deferred errors:{Environment.NewLine}{{errors}}", errors)
                        | DeferredErrors (data, errors, path) ->
                            logger.LogInformation (
                                "Produced GraphQL deferred result with errors for path: {path}",
                                path |> Seq.map string |> Seq.toArray |> Path.Join
                            )

                            if logger.IsEnabled LogLevel.Trace then
                                logger.LogTrace (
                                    $"GraphQL deferred errors:{Environment.NewLine}{{errors}}{Environment.NewLine}GraphQL deferred data:{Environment.NewLine}{{data}}",
                                    errors,
                                    serializeWithOptions data
                                ))

                GQLResponse.Direct (documentId, data, errs)

            | Stream stream ->
                logger.LogInformation (
                    $"Produced stream GraphQL response with documentId = '{{documentId}}' and metadata:{Environment.NewLine}{{metadata}}",
                    documentId,
                    metadata
                )

                if logger.IsEnabled LogLevel.Information then
                    stream
                    |> Observable.add (function
                        | SubscriptionResult data ->
                            logger.LogInformation ("Produced GraphQL subscription result")

                            if logger.IsEnabled LogLevel.Trace then
                                logger.LogTrace ($"GraphQL subscription data:{Environment.NewLine}{{data}}", serializeWithOptions data)
                        | SubscriptionErrors (null, errors) ->
                            logger.LogInformation ("Produced GraphQL subscription errors")
                            logger.LogTrace ($"GraphQL subscription errors:{Environment.NewLine}{{errors}}", errors)
                        | SubscriptionErrors (data, errors) ->
                            logger.LogInformation ("Produced GraphQL subscription result with errors")

                            if logger.IsEnabled LogLevel.Trace then
                                logger.LogTrace (
                                    $"GraphQL subscription errors:{Environment.NewLine}{{errors}}{Environment.NewLine}GraphQL deferred data:{Environment.NewLine}{{data}}",
                                    errors,
                                    serializeWithOptions data
                                ))

                GQLResponse.Stream documentId

            | RequestError errs ->
                logger.LogInformation (
                    $"Produced request error GraphQL response with documentId = '{{documentId}}' and metadata:{Environment.NewLine}{{metadata}}",
                    documentId,
                    metadata
                )

                logger.LogTrace ($"{{number}} errors:{Environment.NewLine}{{errs}}", errs.Length, errs)

                GQLResponse.RequestError (documentId, errs)

        let removeWhitespacesAndLineBreaks (str : string) = str.Trim().Replace ("\r\n", " ")

        /// Check for the conditions that would make this an introspection query
        let isIntrospectionOnlyQuery (ast : Ast.Document) (operationName : string option) : bool =

            let getOperation =
                function
                | OperationDefinition odef -> Some odef
                | _ -> None

            let findOperation doc opName =
                match ast.Definitions |> List.choose getOperation, opName with
                | [ def ], _ -> Some def
                | defs, name -> defs |> List.tryFind (fun def -> def.Name = name)

            match findOperation ast operationName with
            | None ->
                logger.LogTrace ("Document has no operation")
                false
            | Some operation when not (operation.OperationType = Query) ->
                logger.LogTrace ("Document operation is not of type 'Query'")
                false
            | Some operation ->
                let metaTypeFields =
                    seq {
                        "__type"
                        "__schema"
                        "__typename"
                    }
                    |> Set.ofSeq

                let anyFieldIsNotMetaType =
                    // Run through the definitions, stopping and returning true if any name
                    // does not match the ones in metaTypeFields.
                    Seq.exists
                        (function
                        | Field fd ->
                            logger.LogTrace ("Operation Selection is a 'Field' with name='{name}'", fd.Name)
                            not <| metaTypeFields.Contains (fd.Name)
                        | _ ->
                            logger.LogTrace ("Operation Selection is non-'Field' type")
                            false)
                        operation.SelectionSet
                // If all of them passed the test, this is an introspection query.
                not anyFieldIsNotMetaType

        /// Execute default or custom introspection query
        let executeIntrospectionQuery (ast : Ast.Document voption) =
            task {
                let! result =
                    match ast with
                    | ValueNone ->
                        Schema.executor.AsyncExecute (IntrospectionQuery.Definition)
                    | ValueSome ast ->
                        Schema.executor.AsyncExecute (ast)

                return result
            }

        /// Execute the operation for given request
        let executeOperation (operation : GQLRequestContent) (fileMap : IDictionary<string, string> option) (files : IDictionary<string, File> option) =
            task {
                let query = operation.Query
                let operationName = operation.OperationName |> Skippable.toOption
                let variables = operation.Variables |> Skippable.toOption

                operationName
                |> Option.iter (fun on -> logger.LogTrace ($"GraphQL operation name: {{on}}", on))

                let ast = Parser.parse (removeWhitespacesAndLineBreaks query)

                if isIntrospectionOnlyQuery ast operationName then
                    if logger.IsEnabled LogLevel.Trace then
                        logger.LogTrace ($"Executing GraphQL introspection query:{Environment.NewLine}{{query}}", serializeWithOptions query)
                    let! result = executeIntrospectionQuery (ValueSome ast)
                    return result

                else

                    if logger.IsEnabled LogLevel.Trace then
                        logger.LogTrace ($"Executing GraphQL query:{Environment.NewLine}{{query}}", serializeWithOptions query)

                    variables
                    |> Option.iter (fun v -> logger.LogTrace ($"GraphQL variables:{Environment.NewLine}{{variables}}", v))

                    let root = { RequestId = System.Guid.NewGuid () }
                    let executionPlan = Schema.executor.CreateExecutionPlan (ast, ?operationName = operationName)
                    let! result = Schema.executor.AsyncExecute (executionPlan, root, ?variables = variables, ?fileMap = fileMap, ?files = files)
                    return result
            }


        /// Check if the request contains a body
        let hasRequestBody (req : HttpRequest) =
            if req.Body.CanSeek then
                req.Body.Length > 0L |> Task.FromResult
            else
                task {
                    // EnableBuffering allows us to read the Body even if it's been read already somewhere else.
                    // See https://devblogs.microsoft.com/dotnet/re-reading-asp-net-core-request-bodies-with-enablebuffering/
                    req.EnableBuffering ()
                    let body = req.Body
                    let buffer = Array.zeroCreate 1
                    // All we need to ask for is the first byte.
                    let! bytesRead = body.ReadAsync (buffer, 0, 1)
                    body.Seek (0, SeekOrigin.Begin) |> ignore
                    return bytesRead > 0
                }

        /// Check for the conditions that would make this an introspection query
        let isRequestEmpty (req : HttpRequest) (logger : ILogger)=
            if req.Method = HttpMethods.Get then
                logger.LogTrace ("Request is GET. Must be an introspection query")
                true |> Task.FromResult
            else
                task {
                    let! hasBody = hasRequestBody req
                    if not hasBody then
                        logger.LogTrace ("Request is not GET but has no body. Must be an introspection query")
                        return true
                    else
                        logger.LogTrace ("Request is not GET and has a body")
                        return false
                }

        task {
            let! requestIsEmpty = isRequestEmpty request logger

            /// Returns true if the given HttpRequest is multi-part.
            let isMultipartRequest (req : HttpRequest) =
                not (System.String.IsNullOrEmpty(req.ContentType)) && req.ContentType.Contains("multipart/form-data")

            if requestIsEmpty then
                let! result = executeIntrospectionQuery ValueNone
                let response = result |> toResponse
                return Results.Ok response
    
            elif isMultipartRequest request then

                /// Look for a header specifying the boundary string used in a multi-part form submission and return the string if it exists.
                let getMultipartRequestBoundary (req : HttpRequest) =
                    req.Headers.GetCommaSeparatedValues("Content-Type")
                    |> Seq.map (fun v -> v.TrimStart())
                    |> Seq.tryFind (fun v -> v.Contains("boundary"))
                    |> Option.map (fun v -> v.Remove(0, v.IndexOf('=') + 1))
                    |> Option.map (fun v -> v.Trim('"'))

                match getMultipartRequestBoundary request with
                | Some boundary ->

                    let copyBodyToMemory (req : HttpRequest) =
                        let ms = new MemoryStream(4096)
                        req.Body.CopyTo(ms)
                        ms.Position <- 0L
                        ms

                    return! task {
                        use ms = copyBodyToMemory request
                        let reader = MultipartReader(boundary, ms)
                        let! multiRequest = reader |> MultipartRequest.read ctx.RequestAborted
                        let op = multiRequest.Operation

                        let resultTask = executeOperation op (Some multiRequest.FileMap) (Some multiRequest.Files)
                        let result = resultTask.Result  // TODO: Am I doing this right?

                        let addRequestType (requestType : string) (response : GQLExecutionResult) =
                            let mapper (content : GQLResponseContent) =
                                let dataMapper (data : Output) : Output =
                                    let data = data |> Seq.map (|KeyValue|) |> Map.ofSeq
                                    upcast data.Add("requestType", requestType)
                                match content with
                                | GQLResponseContent.Direct (data, errors) -> Direct (dataMapper data, errors)
                                | GQLResponseContent.Deferred (data, errors, deferred) -> Deferred (dataMapper data, errors, deferred)
                                | _ -> content
                            { Content = mapper response.Content; DocumentId = response.DocumentId; Metadata = response.Metadata }

                        let response = result |> addRequestType "Multipart" |> toResponse
                        return Results.Ok response
                    }
                | None ->
                    let problem = GQLProblemDetails.Create ("Invalid multipart request header: missing boundary value.")
                    let response = GQLResponse.RequestError (0, [problem])
                    return Results.BadRequest response

            else
                let! gqlRequest = ctx.BindJsonAsync<GQLRequestContent> ()
                let! result = executeOperation gqlRequest None None
                let response = result |> toResponse
                return Results.Ok response

        }
        |> ofTaskIResult ctx

    let webApp : HttpHandler = setCorsHeaders >=> choose [ POST; GET ] >=> graphQL
