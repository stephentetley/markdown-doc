// Copyright (c) Stephen Tetley 2018
// License: BSD 3 Clause

namespace PandocOutput.Internal

open System.Text

[<RequireQualifiedAccess>]
module SimpleText = 

    type Text = 
        | Empty
        | Raw of string
        | Horizontal of Text * Text
        static member (+) (a:Text, b:Text) = Horizontal(a,b)

    let renderText1 (doc:Text) : string = 
        let sb = new StringBuilder ()
        let rec work (doc:Text) (cont : unit -> 'a) = 
            match doc with
            | Empty -> cont ()
            | Raw str -> sb.Append(str) |> ignore; cont ()
            | Horizontal(d1,d2) -> 
                work d1 (fun _ ->
                work d2 (fun _ -> 
                cont ()))
        work doc (fun _ -> ()) 
        sb.ToString ()

    let renderText (width:int) (doc:Text) : string list = 
        renderText1 doc |> breaklines width


