// Copyright (c) Stephen Tetley 2018,2019
// License: BSD 3 Clause

namespace MarkdownDoc.Internal



[<RequireQualifiedAccess>]
module MarkdownText = 

    open System.Text
    open MarkdownDoc.Internal

    type MdText = 
        | Empty
        | String of string
        | Horizontal of MdText * MdText
        | Vertical of MdText * MdText

    let renderUnbound (doc:MdText) : string = 
        let sb = new StringBuilder ()
        let rec work (doc:MdText) (cont : unit -> 'a) = 
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

    /// This formats to the supplied line width.
    /// Note the line width is respective to the generated markdown and 
    /// not the final printed / published output.
    let renderBounded (width:int) (doc:MdText) : string list = 
        renderUnbound doc |> breaklines width

    let empty : MdText = Empty

    let space : MdText = String " "



    let beside (x:MdText) (y:MdText) : MdText = 
        match x,y with
        | Empty, d -> d
        | d, Empty -> d
        | d1,d2 -> Horizontal(d1,d2)

    let besideSpace (x:MdText) (y:MdText) : MdText = beside x (beside space y)

    let below (x:MdText) (y:MdText) : MdText = 
        match x,y with
        | Empty, d -> d
        | d, Empty -> d
        | d1,d2 -> Vertical(d1,d2)

    let textlines (lines:MdText list) : MdText = 
        let rec work zs cont = 
            match zs with
            | [] -> cont empty
            | x :: xs ->
                work xs (fun v1 -> 
                cont (below x v1))
        work lines id

    let stringText (source:string) : MdText = 
        match source with
        | "" -> Empty
        | _ -> toLines source |> List.map (fun x -> String(x)) |> textlines