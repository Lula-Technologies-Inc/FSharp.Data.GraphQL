namespace FSharp.Data.GraphQL.Validation

open System.Text.Json.Serialization
open FSharp.Data.GraphQL

type ValidationResult<'Err> =
    | Success
    | ValidationError of 'Err list

[<AutoOpen>]
module ValidationResult =
    let (@@) (res1 : ValidationResult<'Err>) (res2 : ValidationResult<'Err>) : ValidationResult<'Err> =
        match res1, res2 with
        | Success, Success -> Success
        | Success, _ -> res2
        | _, Success -> res1
        | ValidationError e1, ValidationError e2 -> ValidationError (e1 @ e2)

    /// Call the given sequence of validations, accumulating any errors, and return one ValidationResult.
    let collectResults (f : 'T -> ValidationResult<'Err>) (xs : 'T seq) : ValidationResult<'Err> =
        // TODO: Use PSeq
        Seq.fold (fun acc t -> acc @@ (f t)) Success xs

[<AbstractClass; Sealed>]
type AstError =

    static member AsResult(message : string, ?path : FieldPath) =
        [ { Message = message; Path = path |> Skippable.ofOption |> Skippable.map List.rev; Locations = Skip; Extensions = Skip  } ]
        |> ValidationResult.ValidationError
