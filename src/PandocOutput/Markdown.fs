// Copyright (c) Stephen Tetley 2018
// License: BSD 3 Clause

module PandocOutput.Markdown


open System.Text
open PandocOutput.Internal.FormatCombinators


type Alignment = AlignDefault | AlignLeft | AlignCenter | AlignRight

    
type ColumnSpec = 
    { Width: int
      Alignment: Alignment }


type System.Text.StringBuilder with
    member v.AppendLines(lines:string list) : unit = 
        List.iter (fun s -> v.AppendLine(s) |> ignore) lines

/// We might change this to help process tables, blockquotes etc.
type Markdown = Doc

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

    let gridTableRegularSep (columnSpecs:ColumnSpec list) : string = alignmentLineSep columnSpecs '-'
    
    let gridTableLine (columnSpecs:ColumnSpec list) (cellLines:string list) : string = 
        let sb = new StringBuilder("|")
        let rec work (cols:ColumnSpec list) (cells: string list) = 
            match cols, cells with 
            | (s :: ss), (t ::ts) ->
                let cell1 = " " + t.PadRight(s.Width-1 , ' ')
                sb.Append(cell1) |> ignore
                sb.Append('|') |> ignore
                work ss ts
            | _,_ -> sb.ToString ()
        work columnSpecs cellLines

    /// F#'s built-in List.transpose needs perfect input. It cannot handle ragged tables.
    let raggedTranspose (emptyElement:'a) (table:('a list) list) : ('a list) list = 
        let headsOf (table:('a list) list) : 'a list = 
            List.map (fun xs -> match xs with | [] -> emptyElement; | (x::_) -> x) table
        let tailsOf (table:('a list) list) : ('a list) list = 
            List.map (fun xs -> match xs with | (_::ys) -> ys; | _ -> []) table
        let rec work ac rows = 
            if List.forall (fun (x:'a list)  -> List.isEmpty x) rows then 
                List.rev ac
            else 
                let line1 = headsOf rows
                let rest = tailsOf rows
                work (line1::ac) rest
        work [] table

    let tableRow1 (columnSpecs: ColumnSpec list) (row:Markdown list) : string list = 
        let cellStrings = List.map render row
        let cells = List.map2 (fun (spec:ColumnSpec) (s:string) -> breaklines spec.Width s) columnSpecs cellStrings
        let lines = raggedTranspose "" cells
        List.map (gridTableLine columnSpecs) lines

    /// The first row is printed as headers.
    let simpleTable (columnSpecs:ColumnSpec list) (contents: (Markdown list) list) : Markdown = 
        let lineSep = gridTableRegularSep columnSpecs 
        let sb = new System.Text.StringBuilder ()
        let rec work (rows : (Markdown list) list) : unit = 
            match rows with 
            | [] -> ()
            | (y :: ys)  -> 
                let lines = tableRow1 columnSpecs y 
                List.iter (fun (s:string) -> sb.AppendLine(s) |> ignore) lines
                sb.AppendLine(lineSep) |> ignore
                work ys

        // Build the table...
        match contents with
        | [] -> ()
        | [x] -> 
            sb.AppendLine(gridTableHeaderSep columnSpecs) |> ignore
            sb.AppendLines(tableRow1 columnSpecs x)
            sb.AppendLine(lineSep) |> ignore
            ()
        | (x :: xs) -> 
            sb.AppendLine(lineSep) |> ignore
            sb.AppendLines(tableRow1 columnSpecs x) 
            sb.AppendLine(gridTableHeaderSep columnSpecs) |> ignore
            work xs
        formatString <| sb.ToString()

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


let unordList (items:Markdown list) : Markdown = 
    vcat <| List.map (fun (doc:Markdown) -> mdchar '*' +^+ doc) items

