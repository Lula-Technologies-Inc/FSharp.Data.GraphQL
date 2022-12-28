namespace FSharp.Data.GraphQL.Samples.StarWarsApi

open System.Collections.Immutable
open System.IO
open System.Text
open System.Text.Json
open System.Threading.Tasks
open Giraffe
open Microsoft.AspNetCore.Http
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open Microsoft.Extensions.Logging

type HttpHandler = HttpFunc -> HttpContext -> HttpFuncResult

type private GQLRequestContent =
    { Query : string
      // TODO: Add OperationName handling
      OperationName : string voption
      Variables : ImmutableDictionary<string, JsonElement> voption }
      
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
                    logger.LogInformation($"GraphQL toResponse Direct with documentId = '{documentId}', Data = {JsonSerializer.Serialize(data, Json.serializerOptions)}")
                    { DocumentId = documentId
                      Data = data
                      Errors = errs }
                | Deferred (data, errs, deferred) ->
                    logger.LogInformation($"GraphQL toResponse Deferred with documentId = '{documentId}', Data = {JsonSerializer.Serialize(data, Json.serializerOptions)}")
                    deferred |> Observable.add (fun d -> printfn "Deferred: %s" (JsonSerializer.Serialize(d, Json.serializerOptions)))
                    { DocumentId = documentId
                      Data = data
                      Errors = errs }
                | Stream data ->
                    logger.LogInformation($"GraphQL toResponse Stream with documentId = '{documentId}', Data = {JsonSerializer.Serialize(data, Json.serializerOptions)}")
                    data |> Observable.add (fun d -> printfn "Subscription data: %s" (JsonSerializer.Serialize(d, Json.serializerOptions)))
                    { DocumentId = documentId
                      Data = null
                      Errors = [] }

            let removeWhitespacesAndLineBreaks (str : string) = str.Trim().Replace ("\r\n", " ")

            let hasData (context: HttpContext) =
                let request = context.Request
                if request.Body.CanSeek then
                    request.Body.Length > 0L
                else
                    request.EnableBuffering()
                    let body = request.Body
                    let buffer = Array.zeroCreate 1
                    let bytesRead = body.Read(buffer, 0, 1)
                    body.Seek(0, SeekOrigin.Begin) |> ignore
                    bytesRead > 0

            if (ctx.Request.Method = HttpMethods.Get || (not <| hasData ctx))
            then
                let! result = Schema.executor.AsyncExecute (Introspection.IntrospectionQuery)
                logger.LogInformation($"GraphQL Result metadata: '{result.Metadata}'")
                let response = toResponse result.DocumentId result.Content
                return Results.Ok response
            else

            let! request = ctx.BindJsonAsync<GQLRequestContent>()
            let query = request.Query

            return!
                match request.Variables with
                | ValueSome variables -> task {
                        logger.LogInformation($"GraphQL Received query: '{query}', variables = {variables}")
                        let query = removeWhitespacesAndLineBreaks query
                        let root = { RequestId = System.Guid.NewGuid().ToString () }
                        let! result = Schema.executor.AsyncExecute (query, root, variables)
                        logger.LogInformation($"GraphQL Result metadata: '{result.Metadata}'")
                        let response = toResponse result.DocumentId result.Content
                        return Results.Ok response
                    }
                | ValueNone -> task {
                        logger.LogInformation($"GraphQL Received query: '{query}', variables = none")
                        let query = removeWhitespacesAndLineBreaks query
                        let! result = Schema.executor.AsyncExecute (query)
                        logger.LogInformation($"GraphQL Result metadata: '{result.Metadata}'")
                        let response = toResponse result.DocumentId result.Content
                        return Results.Ok response
                    }
        }
        |> ofTaskIResult ctx

    let webApp : HttpHandler = setCorsHeaders >=> choose [ POST; GET ] >=> graphQL
