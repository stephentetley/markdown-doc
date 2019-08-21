// Copyright (c) Stephen Tetley 2018,2019
// License: BSD 3 Clause

#r "netstandard"
#r "System.Xml.Linq.dll"

// Use FSharp.Data for CSV reading
#I @"C:\Users\stephen\.nuget\packages\fsharp.data\3.1.1\lib\netstandard2.0"
#r @"FSharp.Data.dll"
open FSharp.Data

#I @"C:\Users\stephen\.nuget\packages\slformat\1.0.2-alpha-20190721\lib\netstandard2.0"
#r "SLFormat.dll"

#load "..\src\MarkdownDoc\Internal\Common.fs"
#load "..\src\MarkdownDoc\Internal\GridTable.fs"
#load "..\src\MarkdownDoc\Internal\SimpleDoc.fs"
#load "..\src\MarkdownDoc\Internal\Doc.fs"
#load "..\src\MarkdownDoc\Markdown\Text.fs"
#load "..\src\MarkdownDoc\Markdown\Block.fs"
#load "..\src\MarkdownDoc\Markdown\Table.fs"
#load "..\src\MarkdownDoc\Pandoc\Invoke.fs"

open MarkdownDoc.Markdown
open MarkdownDoc.Pandoc

let safeName (input:string) : string = 
    let parens = ['('; ')'; '['; ']'; '{'; '}']
    let bads = ['\\'; '/'; ':'; '?'] 
    let white = ['\n'; '\t']
    let ans1 = List.fold (fun (s:string) (c:char) -> s.Replace(c.ToString(), "")) input parens
    let ans2 = List.fold (fun (s:string) (c:char) -> s.Replace(c,'_')) ans1 bads
    let ans3 = List.fold (fun (s:string) (c:char) -> s.Replace(c,'_')) ans2 white
    ans3.Trim() 


// ****************************************************************************
// Build a document
// This example has some conditional processing


type WorkType = Commission | Revisit
type PhaseType = Phase1 | Phase2

type Item = 
    { Uid: string 
      Name: string
      Work: WorkType
      Phase: PhaseType }

let nbsp2 : Markdown = nbsp ^!!^ nbsp

let logo : Markdown = 
    markdownText (inlineImage "" @"include/YW-logo.jpg" None)

let title1 (phase:PhaseType) : Markdown = 
    let caption = 
        match phase with
        | Phase1 -> "T0877 Hawkeye 2 to Point Blue Asset Replacement (Phase 1)"
        | Phase2 -> "T0942 Hawkeye 2 to Point Blue Asset Replacement (Phase 2)"
    h1 (text caption)

let title2 (sai:string) (name:string) : Markdown = 
    h2 (text sai ^+^ text name)

let partners : Markdown = 
    let partnerLine name desc : Markdown = 
        let body : Text = (doubleAsterisks <| text name) ^+^ text desc 
        markdownText body
    [ h2 (text "Asset Replacement Project Partners")
    ; partnerLine "Metasphere" "Project Delivery"
    ; partnerLine "OnSite" "Installation and Commmissioning"
    ] |> vsep

let workDoc (work:WorkType) : Text = 
    match work with
    | Commission -> text "Point Blue Installation / Commissioning Form"
    | Revisit -> text "Point Blue Revisit Form"

let contents (work:WorkType) : Markdown = 
    h3 (text "Contents") ^!!^ unorderedList [ markdownText (workDoc work)]

let makeDoc (item:Item) : Markdown = 
    [ logo
    ; nbsp2
    ; title1 item.Phase
    ; nbsp2
    ; title2 item.Uid item.Name
    ; nbsp2
    ; partners
    ; nbsp2
    ; contents item.Work
    ] |> vsep


// ****************************************************************************
// Generate output from a work list

type InputTable = 
    CsvProvider<Sample = @"..\data\coversheet-schema.csv",
                 HasHeaders = true >

type InputRow = InputTable.Row


/// Alternatively = @"..\data\coversheet-schema.csv"
let inputFile = @"G:\work\Projects\events2\point-blue\prolog\output.csv"

let rows () : InputRow list  = InputTable.Load(inputFile).Rows |> Seq.toList


let rowToItem (row:InputRow) : option<Item> = 
    let work = 
        match row.``Commission or Revisit`` with
        | "COMMISSION" -> Some Commission
        | "REVISIT" -> Some Revisit
        | _ -> None
    let phase = 
        match row.Phase with
        | "PHASE ONE" -> Some Phase1
        | "PHASE TWO" -> Some Phase2
        | _ -> None
    Option.map2 (fun a b -> { Uid = row.``Sai Number``
                            ; Name = row.``Site Name``
                            ; Work = a
                            ; Phase = b }) work phase


let items () : Item list = 
    List.choose id << List.map rowToItem <| rows ()



// let itemSample = { Uid = "SAI00004096"; Name = "WALDORF/2 SLD"; Work = Commission; Phase = Phase1}

let makeFileName (item:Item) (extension:string) : string = 
    let pid = 
        match item.Phase with
        | Phase1 -> "P1"
        | Phase2 -> "P2"
    let work = 
        match item.Work with
        | Commission -> "C"
        | Revisit -> "R"
    sprintf "%s-%s%s-cover.%s" (safeName item.Name) pid work extension

let outputItem (item:Item) : unit = 
    let cwd = @"G:\work\Projects\events2\point-blue\markdown"
    let phase = 
        match item.Phase with
        | Phase1 -> "phase1"
        | Phase2 -> "phase2"
    let mdPath = System.IO.Path.Combine(cwd, "output", phase, makeFileName item "md")
    let docxPath = System.IO.Path.Combine(cwd, "output", phase, makeFileName item "docx")
    let htmlPath = System.IO.Path.Combine(cwd, "output", phase, makeFileName item "html")
    let txtPath = System.IO.Path.Combine(cwd, "output", phase, makeFileName item "txt")    
    let stylesDoc = System.IO.Path.Combine(cwd, @"include/custom-reference1.docx")
    let doc:Markdown = makeDoc item
    writeMarkdown 120 doc mdPath
    runPandocDocx true cwd mdPath docxPath (Some stylesDoc) pandocDefaults |> ignore
    runPandocHtml5 true cwd mdPath htmlPath (Some item.Name) pandocDefaults |> ignore
    runPandocPlain true cwd mdPath txtPath pandocDefaults |> ignore




let main () =
    items () |> List.iter outputItem


// ****************************************************************************
// Generate output from a work list

type MissedTable = 
    CsvProvider<Sample = @"G:\work\Projects\events2\point-blue\missing.csv",
                 HasHeaders = true >

type MissedRow = MissedTable.Row



let missedRows () : MissedRow list  = (new MissedTable()).Rows |> Seq.toList



let missedRowToItem (row:MissedRow) : option<Item> = 
    let work = 
        match row.Work with
        | "commissioning" -> Some Commission
        | "revisit" -> Some Revisit
        | _ -> None
    let phase = 
        match row.Scheme with
        | "T0877" -> Some Phase1
        | "T0942" -> Some Phase2
        | _ -> None
    Option.map2 (fun a b -> { Uid = row.``SAI number``
                            ; Name = row.``New AI2 Name``
                            ; Work = a
                            ; Phase = b }) work phase



let missedItems () : Item list = 
    List.choose id << List.map missedRowToItem <| missedRows ()

let main2 () =
    missedItems () |> List.iter outputItem