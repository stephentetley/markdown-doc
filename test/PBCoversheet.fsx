// Copyright (c) Stephen Tetley 2018
// License: BSD 3 Clause


// Use FSharp.Data for CSV reading
#I @"..\packages\FSharp.Data.3.0.0-beta4\lib\net45"
#r @"FSharp.Data.dll"
open FSharp.Data


#load "..\src\MarkdownDoc\Internal\Common.fs"
#load "..\src\MarkdownDoc\Internal\SimpleText.fs"
#load "..\src\MarkdownDoc\Internal\Tile.fs"
#load "..\src\MarkdownDoc\Markdown.fs"
#load "..\src\MarkdownDoc\PandocInvoke.fs"

open MarkdownDoc
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

let nbsp2 : Markdown = 
    preformatted [nbsp; nbsp]

let logo : Markdown = 
    tile (inlineImage (text " ") @"include/YW-logo.jpg" None)

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
        tile body
    concat [ h2 (text "Asset Replacement Project Partners")
           ; partnerLine "Metasphere" "Project Delivery"
           ; partnerLine "OnSite" "Installation and Commmissioning"
           ]

let workDoc (work:WorkType) : Text = 
    match work with
    | Commission -> text "Point Blue Installation / Commissioning Form"
    | Revisit -> text "Point Blue Revisit Form"

let contents (work:WorkType) : Markdown = 
    h3 (text "Contents") ^@^ unordList [ tile (workDoc work)]

let makeDoc (item:Item) : Markdown = 
    concat [ logo
           ; nbsp2
           ; title1 item.Phase
           ; nbsp2
           ; title2 item.Uid item.Name
           ; nbsp2
           ; partners
           ; nbsp2
           ; contents item.Work
           ]



let generateDocx (workingDirectory:string) (mdInputPath:string) (outputDocxName:string) :unit  =
    let stylesDoc = @"include/custom-reference1.docx" 
    runPandocDocx workingDirectory mdInputPath outputDocxName stylesDoc []


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
    let mdPath = System.IO.Path.Combine("output", phase, makeFileName item "md")
    let docxPath = System.IO.Path.Combine("output", phase, makeFileName item "docx")
    let mdOutPath = System.IO.Path.Combine(cwd, mdPath)
    ((makeDoc item):Markdown).Save(columnWidth = 80, outputPath = mdOutPath)
    generateDocx cwd mdPath docxPath



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