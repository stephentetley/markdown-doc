// Copyright (c) Stephen Tetley 2018
// License: BSD 3 Clause

module PandocOutput.Internal.TileDoc

open System.Text

open PandocOutput.Internal.Common

/// This is not a "pretty printer" as it makes no effort to "fit" the output.
type Text = 
    | Empty
    | Raw of string
    | Horizontal of Text * Text
    static member (+) (a:Text, b:Text) = Horizontal(a,b)

let renderText (width:int) (doc:Text) : string list = 
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
    sb.ToString () |> breaklines width

let empty : Text = Empty

let character (ch:char) : Text = Raw <| ch.ToString()

/// TODO - should probably also have a version that does escaping...
let rawtext (text:string) : Text = Raw text



// Note (@) appears to only operate on lists (it doesn't work for array...).
// This implies we should use another combinator symbol for joining things.


/// Horizontal concat with a separating space 
let (<+>) (d1:Text) (d2:Text) : Text = 
   d1 + character ' ' + d2

let bang : Text = character '!'
let colon : Text = character ':'
let space : Text = character ' '

let enclose (left:Text) (right:Text) (d1:Text) : Text = 
    left + d1 + right


let parens (source:Text) : Text = 
    enclose (character '(') (character ')') source

let squareBrackets (source:Text) : Text = 
    enclose (character '[') (character ']') source

/// Can be used for inlining links.
let angleBrackets (source:Text) : Text = 
    enclose (character '<') (character '>') source

let singleQuotes (source:Text) : Text = 
    enclose (character '\'') (character '\'') source

let doubleQuotes (source:Text) : Text = 
    enclose (character '"') (character '"') source

/// Emphasis
let asterisks (source:Text) : Text = 
    enclose (character '*') (character '*') source

/// Emphasis
let underscores (source:Text) : Text = 
    enclose (character '_') (character '_') source

/// Strong emphasis
let doubleAsterisks (source:Text) : Text = 
    enclose (rawtext "**") (rawtext "**") source

/// Strong emphasis
let doubleUnderscores (source:Text) : Text = 
    enclose (rawtext "__") (rawtext "__") source

/// Backticks for inline code.
let backticks (source:Text) : Text = 
    enclose (character '`') (character '`') source

/// Backticks for inline code.
let doubleBackticks (source:Text) : Text = 
    enclose (rawtext "``") (rawtext "``") source

/// [A link](/path/to)
///
/// [A link](/path/to "Title") 
let inlineLink (altText:Text) (path:string) (title:option<string>) : Text = 
    let title1  = 
        match title with
        | None -> empty
        | Some ss -> space + doubleQuotes (rawtext ss)
    (squareBrackets altText) + parens (rawtext path + title1)


let defLinkReference (identifier:string) (path:string) (title:option<string>) : Text = 
    let title1  = 
        match title with
        | None -> empty
        | Some ss -> space + doubleQuotes (rawtext ss)
    squareBrackets (rawtext identifier) + colon <+> angleBrackets (rawtext path) + title1

let useLinkReference (altText:Text) (identifier:string) : Text = 
    squareBrackets altText + squareBrackets (rawtext identifier)

let inlineImage (altText:Text) (path:string) (title:option<string>) : Text = 
    let title1  = 
        match title with
        | None -> empty
        | Some ss -> space + doubleQuotes (rawtext ss)
    bang + (squareBrackets altText) + parens (rawtext path + title1)

let defImageReference (identifier:string) (path:string) (title:option<string>) : Text = 
    let title1  = 
        match title with
        | None -> empty
        | Some str -> space + doubleQuotes (rawtext str)
    squareBrackets (rawtext identifier) + colon <+> rawtext path + title1

let useImageReference (altText:Text) (identifier:string) : Text = 
    bang + (squareBrackets altText) + (squareBrackets <| rawtext identifier)



//// Prefix2 is for e.g list items where the first prefix is * and the subsequent ones are indents.
//// Can this representation accommodate tables?
//type Tile = 
//    | VDoc of string list
//    | Prefix of string * Tile
//    | Suffix of Tile * string
//    | Prefix2 of string * string * Tile
