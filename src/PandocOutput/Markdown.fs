// Copyright (c) Stephen Tetley 2018
// License: BSD 3 Clause

module PandocOutput.Markdown


open System.Text
open PandocOutput.Internal.FormatCombinators


type Alignment = AlignDefault | AlignLeft | AlignCenter | AlignRight

    
type ColumnSpec = 
    { Width: int
      Alignment: Alignment }


/// Favour Grid Tables for output.
module GridTableHelpers = 
    type Lines = string list
    type Words = string list



    /// Precondition: source is a single line with only spaces (no tabs/newlines)
    let breakline1 (width:int) (source:string) : string list = 
        let words = source.Split(' ') |> Array.toList
        let makeLine (words:Words) : string = String.concat " " (List.rev words)
        let rec work (acc:Lines) (a1:Words) (pos:int) (ss:Words) =  
            match ss with 
            | [] -> 
                if List.isEmpty a1 then 
                    List.rev acc 
                else List.rev ((makeLine a1)::acc)
            | (w::ws) -> 
                if pos + 1 + w.Length > width then
                    // The first word encountered might be too long..
                    if List.isEmpty a1 then 
                        work (w::acc) [] 1 ws
                    else
                        work ((makeLine a1)::acc) [w] w.Length ws
                else 
                    work acc (w::a1) (pos + 1 + w.Length) ws
        work [] [] 0 words

    let breaklines (width:int) (source:string) : string list = 
        let lines = source.Split([| '\n' |]) 
        Array.map (breakline1 width) lines |> List.concat 

    
    let gridTableLineSep (columnSpecs:ColumnSpec list) : string = 
        let sb = new StringBuilder("+")
        let rec work (cols:ColumnSpec list) = 
            match cols with 
            | [] -> sb.ToString()
            | (s :: ss) ->
                sb.Append(String.replicate s.Width "-") |> ignore
                sb.Append('+') |> ignore
                work ss
        work columnSpecs

    let alignmentLineSep1 (width:int) (align:Alignment) (ch:char) : string = 
        match align with
        | AlignDefault -> String.replicate width (ch.ToString())
        | AlignLeft -> ":" + String.replicate (width-1) (ch.ToString())
        | AlignCenter -> ":" + String.replicate (width-2) (ch.ToString()) + ":"
        | AlignRight -> String.replicate (width-1) (ch.ToString()) + ":"


    let alignmentLineSep (columnSpecs:ColumnSpec list) (ch:char) : string = 
        let sb = new StringBuilder("+")
        let rec work (cols:ColumnSpec list) = 
            match cols with 
            | [] -> sb.ToString()
            | (s :: ss) ->
                let sep1 = alignmentLineSep1 s.Width s.Alignment ch
                sb.Append(sep1) |> ignore
                sb.Append('+') |> ignore
                work ss
        work columnSpecs

    let gridTableHeaderSep (columnSpecs:ColumnSpec list) : string = alignmentLineSep columnSpecs '='

    let gridTableNoHeaderSep (columnSpecs:ColumnSpec list) : string = alignmentLineSep columnSpecs '-'

/// We might change this to help process tables, blockquotes etc.
type Markdown = Doc


let render = PandocOutput.Internal.FormatCombinators.render
let testRender = PandocOutput.Internal.FormatCombinators.testRender

let inline private mdchar (ch:char) : Markdown = formatChar ch
let inline private mdstring (str:string) : Markdown = formatString str

let plaintext (text:string) : Markdown = 
    mdstring text


/// TODO - we should be careful about rexports from FormatCombinators.
/// We might want to change the type of ``Markdown``.
let (+++) = PandocOutput.Internal.FormatCombinators.(+++)
let backslash = PandocOutput.Internal.FormatCombinators.backslash

let h1 (source:Markdown) : Markdown = 
    mdchar '#' +^+ source

let h2 (source:Markdown) : Markdown = 
    mdstring "##" +^+ source

let h3 (source:Markdown) : Markdown = 
    mdstring "###" +^+ source

let h4 (source:Markdown) : Markdown = 
    mdstring "####" +^+ source

let h5 (source:Markdown) : Markdown = 
    mdstring "#####" +^+ source

let h6 (source:Markdown) : Markdown = 
    mdstring "######" +^+ source

/// Emphasis
let asterisks (source:Markdown) : Markdown = 
    enclose (mdchar '*') (mdchar '*') source

/// Emphasis
let underscores (source:Markdown) : Markdown = 
    enclose (mdchar '_') (mdchar '_') source

/// Strong emphasis
let doubleAsterisks (source:Markdown) : Markdown = 
    enclose (mdstring "**") (mdstring "**") source

/// Strong emphasis
let doubleUnderscores (source:Markdown) : Markdown = 
    enclose (mdstring "__") (mdstring "__") source


let strikethrough (source:Markdown) : Markdown = 
    enclose (mdstring "~~") (mdstring "~~") source

let raw (body:string) : Markdown = 
    mdstring "```" @@@ plaintext body @@@ mdstring "```"

let raw2 (identifier:string) (body:string) : Markdown = 
    (mdstring "```"  +++ mdstring identifier) @@@ plaintext body @@@ mdstring "```"

let docxPagebreak : Markdown = 
    let source : string = 
        [ "<w:p>"
        ; "  <w:r>"
        ; "    <w:br w:type=\"page\"/>"
        ; "  </w:r>"
        ; "</w:p>"
        ] |> String.concat "\n" 
    raw2 "{=openxml}" source