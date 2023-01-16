// The MIT License (MIT)
// Copyright (c) 2016 Bazinga Technologies Inc

[<AutoOpen>]
module FSharp.Data.GraphQL.Errors

open System
open System.Collections.Generic
open System.Collections.Immutable
open FSharp.Collections.Immutable

let getArrayErrors (array: GQLResult<'t> seq) =
    array
    |> Seq.mapi (fun i result -> result |> Result.mapError (fun errs -> struct (i, errs)))
    |> Seq.choose
        (function
        | Ok _ -> None
        | Error struct (i, errs) ->
            { new IGQLError with member _.Message = $"Cannot coerce item '%i{i}'. See details in 'extensions'"
                interface IGQLErrorExtensions with
                member _.Extensions =
                    HashMap.singleton ("detail", errs |> box)
                    :> IReadOnlyDictionary<string, obj> |> ValueSome }
            |> Some)
    |> Seq.toList

let getArrayValues(array: GQLResult<'t> seq) =
    array
    |> Seq.map (function Ok v -> v | Error _ -> raise <| ArgumentException())
    |> Seq.toArray

let (|ArrayErrors|ArrayValues|) (object: GQLResult<'t> seq) =
    let errors = object |> getArrayErrors
    if not <| List.isEmpty errors then ArrayErrors errors
    else
        let values = object |> getArrayValues
        ArrayValues values

let getListValues(array: GQLResult<'t> seq) =
    array
    |> Seq.map (function Ok v -> v | Error _ -> raise <| ArgumentException())
    |> Seq.toList

let (|ListErrors|ListValues|) (object: GQLResult<'t> seq) =
    let errors = object |> getArrayErrors
    if not <| List.isEmpty errors then ListErrors errors
    else
        let values = object |> getListValues
        ListValues values

let getObjectErrors (object: IReadOnlyDictionary<string, GQLResult<'t>>) =
    object
    |> Seq.choose
        (fun kv ->
            match kv.Value with
            | Ok _ -> None
            | Error errs ->
                { new IGQLError with member _.Message = $"Cannot coerce field '%s{kv.Key}'. See details in 'extensions'"
                    interface IGQLErrorExtensions with
                    member _.Extensions =
                        HashMap.singleton ("detail", errs |> box)
                        :> IReadOnlyDictionary<string, obj> |> ValueSome }
                |> Some)
    |> Seq.toList

let getObjectValues(object: IReadOnlyDictionary<string, GQLResult<'t>>) =
    object
    |> Seq.map (fun kv -> match kv.Value with Ok value -> kvp kv.Key value | Error _ -> raise <| ArgumentException())
    |> Seq.toHashMap

let (|ObjectErrors|ObjectValues|) (object: IReadOnlyDictionary<string, GQLResult<'t>>) =
    let errors = object |> getObjectErrors
    if not <| List.isEmpty errors then ObjectErrors errors
    else
        let values = object |> getObjectValues
        ObjectValues values
