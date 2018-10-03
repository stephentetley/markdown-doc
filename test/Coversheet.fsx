﻿// Copyright (c) Stephen Tetley 2018
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
    tile (inlineImage (rawtext " ") @"include/YW-logo.jpg" None)

let title1 (phase:PhaseType) : Markdown = 
    let caption = 
        match phase with
        | Phase1 -> "T0877 Hawkeye 2 to Point Blue Asset Replacement (Phase 1)"
        | Phase2 -> "T0942 Hawkeye 2 to Point Blue Asset Replacement (Phase 2)"
    h1 (rawtext caption)

let title2 (sai:string) (name:string) : Markdown = 
    h2 (rawtext sai <+> rawtext name)

let partners : Markdown = 
    let partnerLine name desc : Markdown = 
        let body : Text = (doubleAsterisks <| rawtext name) <+> rawtext desc 
        tile body
    concat [ h2 (rawtext "Asset Replacement Project Partners")
           ; partnerLine "Metasphere" "Project Delivery"
           ; partnerLine "OnSite" "Installation and Commmissioning"
           ]

let workDoc (work:WorkType) : Text = 
    match work with
    | Commission -> rawtext "Point Blue Installation / Commissioning Form"
    | Revisit -> rawtext "Point Blue Revisit Form"

let contents (work:WorkType) : Markdown = 
    h3 (rawtext "Contents") + unordList [ tile (workDoc work)]

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



let generateDocx (workingDirectory:string) (mdInputPath:string) (outputDocxName:string)  =
    let opts = 
        { ReferenceDoc = Some @"include/custom-reference1.docx" 
          DocxExtensions = extensions ["table_captions"] }
    runPandocDocx workingDirectory mdInputPath opts outputDocxName 


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



let itemSample = { Uid = "SAI00004096"; Name = "WALDORF/2 SLD"; Work = Commission; Phase = Phase1}

let outputItem (item:Item) : unit = 
    let cwd = @"G:\work\Projects\events2\point-blue\markdown"
    let name1 = safeName item.Name
    let mdPath = System.IO.Path.Combine("output", sprintf "%s-cover.md" name1)
    let docxPath = System.IO.Path.Combine("output", sprintf "%s-cover.docx" name1)
    let mdOutPath = System.IO.Path.Combine(cwd, mdPath)
    renderFile 80 mdOutPath (makeDoc itemSample)
    generateDocx cwd mdPath docxPath


let demo01 () = 
    outputItem itemSample

let main () =
    items () |> List.iter outputItem

