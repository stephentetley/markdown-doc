// Copyright (c) Stephen Tetley 2018
// License: BSD 3 Clause

namespace PandocOutput.Internal

open System.Text

[<RequireQualifiedAccess>]
module SimpleText = 

    type SimpleText = 
        | Empty
        | Raw of string
        | Horizontal of SimpleText * SimpleText
        static member (+) (a:SimpleText, b:SimpleText) = Horizontal(a,b)

    let renderText1 (doc:SimpleText) : string = 
        let sb = new StringBuilder ()
        let rec work (doc:SimpleText) (cont : unit -> 'a) = 
            match doc with
            | Empty -> cont ()
            | Raw str -> sb.Append(str) |> ignore; cont ()
            | Horizontal(d1,d2) -> 
                work d1 (fun _ ->
                work d2 (fun _ -> 
                cont ()))
        work doc (fun _ -> ()) 
        sb.ToString ()

    let renderText (width:int) (doc:SimpleText) : string list = 
        renderText1 doc |> breaklines width

    let empty : SimpleText = Empty

    let character (ch:char) : SimpleText = Raw <| ch.ToString()

    /// TODO - should probably also have a version that does escaping...
    let rawtext (text:string) : SimpleText = Raw text
