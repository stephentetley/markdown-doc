// Copyright (c) Stephen Tetley 2018
// License: BSD 3 Clause

module PandocOutput.Internal.TileDoc

open System.Text

open PandocOutput.Internal.Common

/// This is not a "pretty printer" as it makes no effort to "fit" the output.
type HDoc = 
    | Empty
    | HText of string
    | HDoc of HDoc * HDoc
    static member (+) (a:HDoc, b:HDoc) = HDoc(a,b)

let renderHDoc (width:int) (doc:HDoc) : string list = 
    let sb = new StringBuilder ()
    let rec work (doc:HDoc) (cont : unit -> 'a) = 
        match doc with
        | Empty -> cont ()
        | HText str -> sb.Append(str) |> ignore; cont ()
        | HDoc(d1,d2) -> 
            work d1 (fun _ ->
            work d2 (fun _ -> 
            cont ()))
    work doc (fun _ -> ()) 
    sb.ToString () |> breaklines width

let character (ch:char) : HDoc = HText (ch.ToString())
let rawtext (text:string) : HDoc = HText text
        
let enclose (left:HDoc) (right:HDoc) (d1:HDoc) : HDoc = 
    left + d1 + right

/// A single space
let space : HDoc = character ' '

let dot : HDoc = character '.'
let semi : HDoc = character ';'
let colon : HDoc = character ':'
let comma : HDoc = character ','
let backslash : HDoc = character '\\'
let forwardslash : HDoc = character '/'

let underscore : HDoc = character '_'


// Note (@) appears to only operate on lists (it doesn't work for array...).
// This implies we should use another 

// Prefix2 is for e.g list items where the first prefix is * and the subsequent ones are indents.
// Can this representation accommodate tables?
type VDoc = 
    | VDoc of string list
    | Prefix of string * VDoc
    | Suffix of VDoc * string
    | Prefix2 of string * string * VDoc
