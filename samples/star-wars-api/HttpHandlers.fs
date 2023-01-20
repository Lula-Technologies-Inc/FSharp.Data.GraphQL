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

type HttpHandler = HttpFunc -> HttpContext -> HttpFuncResult

type private GQLRequestContent =
    { Query : string
      // TODO: Add OperationName handling
      OperationName : string voption
      Variables : ImmutableDictionary<string, JsonElement> voption }

module HttpHandlers =

    let ofTaskIResult ctx (taskRes: Task<IResult>) : HttpFuncResult = task {
        let! res = taskRes
        do! res.ExecuteAsync(ctx)
        return Some ctx
    }

    /// Set CORS to allow external servers (React samples) to call this API
    let setCorsHeaders : HttpHandler =
        setHttpHeader "Access-Control-Allow-Origin" "*"
        >=> setHttpHeader "Access-Control-Allow-Headers" "content-type"

    let private graphQL (next : HttpFunc) (ctx : HttpContext) =
        task {

            // TODO: validate the result
            let toResponse documentId =
                function
                | RequestError errs -> GQLResponse.RequestError (documentId, errs)
                | Direct   (data, errs) -> GQLResponse.Direct (documentId, data, errs)
                | Deferred (data, errs, deferred) ->
                    // TODO: Print to logger
                    deferred |> Observable.add (fun d -> printfn "Deferred: %s" (JsonSerializer.Serialize(d, Json.serializerOptions)))
                    GQLResponse.Direct (documentId, data, errs)
                | Stream stream ->
                    // TODO: Print to logger
                    stream |> Observable.add (fun d -> printfn "Subscription data: %s" (JsonSerializer.Serialize(d, Json.serializerOptions)))
                    GQLResponse.Stream documentId

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
                let! result = Schema.executor.AsyncExecute (Introspection.IntrospectionQuery)
                printfn "Result metadata: %A" result.Metadata
                let response = toResponse result.DocumentId result.Content
                return Results.Ok response
            else

            let! request = ctx.BindJsonAsync<GQLRequestContent>()
            let query = request.Query

            return!
                match request.Variables with
                | ValueSome variables -> task {
                        printfn "Received query: %s" query
                        printfn "Received variables: %A" variables
                        let query = removeWhitespacesAndLineBreaks query
                        let root = { RequestId = System.Guid.NewGuid().ToString () }
                        let! result = Schema.executor.AsyncExecute (query, root, variables)
                        printfn "Result metadata: %A" result.Metadata
                        let response = toResponse result.DocumentId result.Content
                        return Results.Ok response
                    }
                | ValueNone -> task {
                        printfn "Received query: %s" query
                        let query = removeWhitespacesAndLineBreaks query
                        let! result = Schema.executor.AsyncExecute (query)
                        printfn "Result metadata: %A" result.Metadata
                        let response = toResponse result.DocumentId result.Content
                        return Results.Ok response
                    }
        }
        |> ofTaskIResult ctx

    let webApp : HttpHandler = setCorsHeaders >=> choose [ POST; GET ] >=> graphQL
