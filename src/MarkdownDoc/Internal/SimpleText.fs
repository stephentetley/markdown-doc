// Copyright (c) Stephen Tetley 2018,2019
// License: BSD 3 Clause

namespace MarkdownDoc.Internal



[<RequireQualifiedAccess>]
module SimpleText = 

    open System.Text
    open MarkdownDoc.Internal.Common

    type Text = 
        | Empty
        | String of string
        | Horizontal of Text * Text
        | Vertical of Text * Text

    let renderText1 (doc:Text) : string = 
        let sb = new StringBuilder ()
        let rec work (doc:Text) (cont : unit -> 'a) = 
            match doc with
            | Empty -> cont ()
            | String str -> 
                sb.Append(str) |> ignore
                cont ()
            | Horizontal(d1,d2) -> 
                work d1 (fun _ ->
                work d2 (fun _ -> 
                cont ()))
            | Vertical(d1,d2) -> 
                work d1 (fun _ ->
                sb.AppendLine() |> ignore
                work d2 (fun _ -> 
                cont ()))
        work doc (fun _ -> ()) 
        sb.ToString ()


    let renderText (width:int) (doc:Text) : string list = 
        renderText1 doc |> breaklines width

    let empty : Text = Empty

    let space : Text = String " "



    let beside (x:Text) (y:Text) : Text = 
        match x,y with
        | Empty, d -> d
        | d, Empty -> d
        | d1,d2 -> Horizontal(d1,d2)

    let besideSpace (x:Text) (y:Text) : Text = beside x (beside space y)

    let private below (x:Text) (y:Text) : Text = 
        match x,y with
        | Empty, d -> d
        | d, Empty -> d
        | d1,d2 -> Vertical(d1,d2)

    let textlines (lines:Text list) : Text = 
        let rec work zs cont = 
            match zs with
            | [] -> cont empty
            | x :: xs ->
                work xs (fun v1 -> 
                cont (below x v1))
        work lines id

    let stringText (source:string) : Text = 
        match source with
        | "" -> Empty
        | _ -> lines source |> List.map (fun x -> String(x)) |> textlines