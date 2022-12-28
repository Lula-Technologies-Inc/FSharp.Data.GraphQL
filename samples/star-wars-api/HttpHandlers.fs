namespace FSharp.Data.GraphQL.Samples.StarWarsApi

open System
open System.Collections.Immutable
open System.IO
open System.Text
open System.Text.Json
open System.Threading.Tasks
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Logging
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open Giraffe
open System.Text.Json.Serialization

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

            // TODO: validate the result
            let toResponse documentId =
                function
                | Direct   (data, errs) ->
                    if not <| logger.IsEnabled LogLevel.Trace then
                        logger.LogTrace("Produced direct GraphQL response with documentId = '{documentId}' and data = {data}", documentId, JsonSerializer.Serialize(data, Json.serializerOptions))
                    { DocumentId = documentId
                      Data = data
                      Errors = errs }
                | Deferred (data, errs, deferred) ->
                    if not <| logger.IsEnabled LogLevel.Trace then
                        logger.LogTrace("Produced deferred GraphQL response with documentId = '{documentId}' and data = {data}", documentId, data)
                        deferred |> Observable.add (fun d -> logger.LogTrace("Deferred: {d}", (JsonSerializer.Serialize(d, Json.serializerOptions))))
                    { DocumentId = documentId
                      Data = data
                      Errors = errs }
                | Stream stream ->
                    if not <| logger.IsEnabled LogLevel.Trace then
                        logger.LogTrace("Produced stream GraphQL response with documentId = '{documentId}'", documentId)
                        stream |> Observable.add (fun d -> logger.LogTrace("Subscription data: {d}", (JsonSerializer.Serialize(d, Json.serializerOptions))))
                    { DocumentId = documentId
                      Data = null
                      Errors = [] }

            let removeWhitespacesAndLineBreaks (str : string) = str.Trim().Replace ("\r\n", " ")

            let request = ctx.Request

            let hasData () =
                if request.Body.CanSeek then
                    request.Body.Length > 0L
                else
                    request.EnableBuffering()
                    let body = request.Body
                    let buffer = Array.zeroCreate 1
                    let bytesRead = body.Read(buffer, 0, 1)
                    body.Seek(0, SeekOrigin.Begin) |> ignore
                    bytesRead > 0

            if (request.Method = HttpMethods.Get || not <| hasData ())
            then
                logger.LogInformation("Executing GraphQL introspection query")
                let! result = Schema.executor.AsyncExecute (Introspection.IntrospectionQuery)
                logger.LogTrace("Producing GraphQL introspection result with documentId = '{documentId}' and data = {data} and metadata = {metadata}", result.DocumentId, result.Content, result.Metadata)
                let response = toResponse result.DocumentId result.Content
                return Results.Ok response
            else

            //let! request = ctx.BindJsonAsync<GQLRequestContent>()
            let! request = JsonSerializer.DeserializeAsync<GQLRequestContent>(ctx.Request.Body, Json.serializerOptions)
            let query = request.Query

            logger.LogTrace($"Executing GraphQL query:{Environment.NewLine}{{query}}", query)
            let operationName = request.OperationName |> Skippable.toOption
            operationName |> Option.iter (fun on -> logger.LogTrace("GraphQL operation name: '{operationName}'", on))
            let variables = request.Variables |> Skippable.toOption
            variables |> Option.iter (fun v -> logger.LogTrace($"GraphQL variables:{Environment.NewLine}{{variables}}", v))

            let root = { RequestId = System.Guid.NewGuid().ToString () }
            let query = removeWhitespacesAndLineBreaks query
            let! result = Schema.executor.AsyncExecute (query, root, ?variables = variables, ?operationName = operationName)
            logger.LogTrace("GraphQL Result metadata: '{metadata}'", result.Metadata)
            let response = toResponse result.DocumentId result.Content
            return Results.Ok response
        }
        |> ofTaskIResult ctx

    let webApp : HttpHandler = setCorsHeaders >=> choose [ POST; GET ] >=> graphQL
