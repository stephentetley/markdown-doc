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

let itemSample = { Uid = "SAI00004096"; Name = "WALDORF/SLD"; Work = Commission; Phase = Phase1}

let demo01 () = 
    let output = @"G:\work\Projects\events2\point-blue\markdown\WALDORF.md"
    renderFile 80 output (makeDoc itemSample)

//let concatOptions (strs:string list) = 
//    String.concat " " <| List.filter (fun ss -> ss<>"") strs

//type DocxOptions = 
//    { ReferenceDoc: option<string>
//      EnabledExtensions: string list 
//      DisabledExtensions: string list }

//let docxCommand (mdInputPath:string) (outputDocxName:string) (options:DocxOptions) = 
//    let referenceDoc = 
//        match options.ReferenceDoc with
//        | None -> ""
//        | Some doc -> sprintf "--reference-doc=%s" doc
//    let parts = 
//        [ referenceDoc
//        ; mdInputPath
//        ; "-f markdown"
//        ; "-t docx+table_captions"  // TODO
//        ; "-s"
//        ; sprintf "-o %s" outputDocxName
//        ]
//    concatOptions parts

// Run Pandoc
// pandoc --reference-doc=include/custom-reference1.docx coversheet.md -f markdown -t docx+table_captions -s -o sample-coversheet.docx


let generateDocx (mdInputPath:string) (outputDocxName:string)  =
    let cwd = @"G:\work\Projects\events2\point-blue\markdown"
    let opts = 
        { ReferenceDoc = Some @"include/custom-reference1.docx" 
          DocxExtensions = extensions ["table_captions"] }
    runPandocDocx cwd mdInputPath opts outputDocxName 

let demoRun1 () =
    generateDocx "WALDORF.md" "WALDORF.docx"

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


