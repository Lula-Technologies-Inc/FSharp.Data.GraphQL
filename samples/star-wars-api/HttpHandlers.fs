namespace FSharp.Data.GraphQL.Samples.StarWarsApi

open System
open System.Collections.Immutable
open System.IO
open System.Text.Json
open System.Text.Json.Serialization
open System.Threading.Tasks
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Http.Json
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Logging
open Microsoft.Extensions.Options
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open Giraffe

type HttpHandler = HttpFunc -> HttpContext -> HttpFuncResult

type private GQLRequestContent =
    { Query : string
      OperationName : Skippable<string>
      Variables : Skippable<ImmutableDictionary<string, JsonElement>> }

module HttpHandlers =

    let rec private moduleType = getModuleType <@ moduleType @>

    let ofIResult ctx (res: IResult) : HttpFuncResult = task {
            do! res.ExecuteAsync(ctx)
            return Some ctx
        }

    let ofTaskIResult ctx (taskRes: Task<IResult>) : HttpFuncResult = task {
        let! res = taskRes
        do! res.ExecuteAsync(ctx)
        return Some ctx
    }

    let setCorsHeaders : HttpHandler =
        setHttpHeader "Access-Control-Allow-Origin" "*"
        >=> setHttpHeader "Access-Control-Allow-Headers" "content-type"

    let private graphQL (next : HttpFunc) (ctx : HttpContext) =
        task {
            let logger = ctx.RequestServices.CreateLogger moduleType
            let jsonSerializerOptions = ctx.RequestServices.GetRequiredService<IOptions<JsonOptions>>().Value.SerializerOptions

            // TODO: validate the result
            let toResponse { DocumentId = documentId; Content = content; Metadata = metadata } =
                match content with
                | Direct (data, errs) ->
                    logger.LogInformation($"Produced direct GraphQL response with documentId = '{{documentId}}' and metadata:{Environment.NewLine}{{metadata}}", documentId, metadata)
                    if not <| logger.IsEnabled LogLevel.Trace then
                        logger.LogTrace($"GraphQL response data:{Environment.NewLine}:{{data}}", JsonSerializer.Serialize(data, jsonSerializerOptions))
                    { DocumentId = documentId
                      Data = data
                      Errors = errs }
                | Deferred (data, errs, deferred) ->
                    logger.LogInformation($"Produced deferred GraphQL response with documentId = '{{documentId}}' and metadata:{Environment.NewLine}{{metadata}}", documentId, metadata)
                    if logger.IsEnabled LogLevel.Information then
                        deferred |> Observable.add
                            (function
                                | DeferredResult (data, path) ->
                                    logger.LogInformation("Produced GraphQL deferred result for path: {path}", path |> Seq.map string |> Seq.toArray |> Path.Join)
                                    if logger.IsEnabled LogLevel.Trace then
                                        logger.LogTrace($"GraphQL deferred data:{Environment.NewLine}{{data}}", (JsonSerializer.Serialize(data, jsonSerializerOptions)))
                                | DeferredErrors (null, errors, path) ->
                                    logger.LogInformation("Produced GraphQL deferred errors for path: {path}", path |> Seq.map string |> Seq.toArray |> Path.Join)
                                    if logger.IsEnabled LogLevel.Trace then
                                        logger.LogTrace($"GraphQL deferred errors:{Environment.NewLine}{{errors}}", errors)
                                | DeferredErrors (data, errors, path) ->
                                    logger.LogInformation("Produced GraphQL deferred result with errors for path: {path}", path |> Seq.map string |> Seq.toArray |> Path.Join)
                                    if logger.IsEnabled LogLevel.Trace then
                                        logger.LogTrace($"GraphQL deferred errors:{Environment.NewLine}{{errors}}{Environment.NewLine}GraphQL deferred data:{Environment.NewLine}{{data}}", errors, (JsonSerializer.Serialize(data, jsonSerializerOptions)))
                            )
                    { DocumentId = documentId
                      Data = data
                      Errors = errs }
                | Stream stream ->
                    logger.LogInformation($"Produced stream GraphQL response with documentId = '{{documentId}}' and metadata:{Environment.NewLine}{{metadata}}", documentId, metadata)
                    if logger.IsEnabled LogLevel.Information then
                        stream |> Observable.add
                            (function
                                | SubscriptionResult data ->
                                    logger.LogInformation("Produced GraphQL subscription result")
                                    if logger.IsEnabled LogLevel.Trace then
                                        logger.LogTrace($"GraphQL subscription data:{Environment.NewLine}{{data}}", (JsonSerializer.Serialize(data, jsonSerializerOptions)))
                                | SubscriptionErrors (null, errors) ->
                                    logger.LogInformation("Produced GraphQL subscription errors")
                                    if logger.IsEnabled LogLevel.Trace then
                                        logger.LogTrace($"GraphQL subscription errors:{Environment.NewLine}{{errors}}", errors)
                                | SubscriptionErrors (data, errors) ->
                                    logger.LogInformation("Produced GraphQL subscription result with errors")
                                    if logger.IsEnabled LogLevel.Trace then
                                        logger.LogTrace($"GraphQL subscription errors:{Environment.NewLine}{{errors}}{Environment.NewLine}GraphQL deferred data:{Environment.NewLine}{{data}}", errors, (JsonSerializer.Serialize(data, jsonSerializerOptions)))
                            )
                    { DocumentId = documentId
                      Data = null
                      Errors = [] }

            let removeWhitespacesAndLineBreaks (str : string) = str.Trim().Replace ("\r\n", " ")

            let request = ctx.Request

            let hasData () = task {
            if request.Body.CanSeek then
                return (request.Body.Length > 0L)
            else
                request.EnableBuffering()
                let body = request.Body
                let buffer = Array.zeroCreate 1
                let! bytesRead = body.ReadAsync(buffer, 0, 1)
                body.Seek(0, SeekOrigin.Begin) |> ignore
                return (bytesRead > 0)
            }
            if (request.Method = HttpMethods.Get)
            then
                logger.LogInformation("Executing GraphQL introspection query")
                let! result = Schema.executor.AsyncExecute (Introspection.IntrospectionQuery)
                let response = result |> toResponse
                return Results.Ok response
            else
            let! hd = hasData()
            if (not hd)
            then
                logger.LogInformation("Executing GraphQL introspection query")
                let! result = Schema.executor.AsyncExecute (Introspection.IntrospectionQuery)
                let response = result |> toResponse
                return Results.Ok response
            else
            
            let! request = ctx.BindJsonAsync<GQLRequestContent>()
            let query = request.Query

            logger.LogTrace($"Executing GraphQL query:{Environment.NewLine}{{query}}", query)
            let operationName = request.OperationName |> Skippable.toOption
            operationName |> Option.iter (fun on -> logger.LogTrace("GraphQL operation name: '{operationName}'", on))
            let variables = request.Variables |> Skippable.toOption
            variables |> Option.iter (fun v -> logger.LogTrace($"GraphQL variables:{Environment.NewLine}{{variables}}", v))
            
            let root = { RequestId = System.Guid.NewGuid().ToString () }
            let query = removeWhitespacesAndLineBreaks query
            let! result = Schema.executor.AsyncExecute (query, root, ?variables = variables, ?operationName = operationName)
            let response = result |> toResponse
            return Results.Ok response
        }
        |> ofTaskIResult ctx

    let webApp : HttpHandler = setCorsHeaders >=> choose [ POST; GET ] >=> graphQL
