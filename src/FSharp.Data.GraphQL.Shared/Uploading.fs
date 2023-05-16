// The MIT License (MIT)
// Copyright (c) 2016 Bazinga Technologies Inc
namespace FSharp.Data.GraphQL.Uploading

open System
open System.IO
open System.Text
open System.Collections.Generic

open FSharp.Data.GraphQL


/// The base type for all GraphQLProvider upload types.
/// Upload types are used in GraphQL multipart request spec, mostly for file uploading features.
type Upload (stream : Stream, fileName : string, ?contentType : string, ?ownsStream : bool) =
    new(bytes : byte [], fileName, ?contentType) =
        let stream = new MemoryStream(bytes)
        match contentType with
        | Some ct -> new Upload(stream, fileName, ct, true)
        | None -> new Upload(stream, fileName, ownsStream = true)

    /// Gets the stream associated to this Upload type.
    member _.Stream = stream

    /// Gets the content type of this Upload type.
    member _.ContentType =
        match contentType with
        | Some ct -> ct
        | None ->
            let ext = Path.GetExtension(fileName)
            match MimeTypes.dict.Force().TryGetValue(ext) with
            | (true, mime) -> mime
            | _ -> "application/octet-stream"

    /// Gets the name of the file which contained on the stream.
    member _.FileName = fileName

    /// Gets a boolean value indicating if this Upload type owns the stream associated with it.
    /// If true, it will dispose the stream when this Upload type is disposed.
    member _.OwnsStream = defaultArg ownsStream false

    interface IDisposable with
        member x.Dispose() = if x.OwnsStream then x.Stream.Dispose()


type File =
    { Name : string
      ContentType : string
      Content : string }
    member x.MakeUpload() =
        let bytes = Encoding.UTF8.GetBytes(x.Content)
        new Upload(bytes, x.Name, x.ContentType)
    static member FromDictionary(dict : IDictionary<string, obj>) =
        { Name = downcast dict.["Name"]
          ContentType = downcast dict.["ContentType"]
          Content = downcast dict.["ContentAsText"] }

type UploadRequest =
    { Single : File
      Multiple : File list
      NullableMultiple : File list option
      NullableMultipleNullable : File option list option }
