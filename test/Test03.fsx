// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

#r "netstandard"

#I @"C:\Users\stephen\.nuget\packages\slformat\1.0.2-alpha-20190721\lib\netstandard2.0"
#r "SLFormat.dll"

#load "..\src\MarkdownDoc\Internal\Common.fs"
#load "..\src\MarkdownDoc\Internal\GridTable.fs"
#load "..\src\MarkdownDoc\Internal\SimpleDoc.fs"
#load "..\src\MarkdownDoc\Internal\Doc.fs"
#load "..\src\MarkdownDoc\Markdown\Text.fs"
#load "..\src\MarkdownDoc\Markdown\Block.fs"
open MarkdownDoc.Internal.GridTable
open MarkdownDoc.Internal.SimpleDoc
open MarkdownDoc.Markdown


let demo01 () =
    let block1 : SimpleDoc = Block [[TextString "Block 1 - Line 1"]; [TextString "Line 2"]]
    let block2 : SimpleDoc = Block [[TextString "Block 2"; TextString " "; TextString "- Line1"]]
    let sdoc = VConcat(block1,block2)
    sdoc |> renderSimpleDoc 80 |> printfn "%s"


// This test should indicate whether table rendering 
// is incorrectly reversing lines.

/// | 1.0 | S Tetley | 18/10/2018 | For EDMS |
let controlTable (author:string) : SimpleDoc = 
    
    let columnSpecs : ColumnSpec list = 
        [  { Width = 30; Alignment = AlignLeft }
        ;  { Width = 30; Alignment = AlignLeft }
        ]
    
    let nowstring = System.DateTime.Now.ToShortDateString()

    let text s = TextString s
    let block lines = Block lines


    let headers : SimpleRow = 
        [ block [[text "Revision"]]; block [[text "Prepared By"]];  block [[text "Date"]]; block [[text "Comments"]]]
    let row1 = 
        [ block [[text "1.0"]]; block [[text author]]; block [[text nowstring]]; block [[text "For EDMS"]]]
    let row2 = [Empty; Empty; Empty; Empty]
    Table(columnSpecs, Some headers, [row1; row2])

let demo02 () = 
    controlTable "S Tetley" |> renderSimpleDoc 80 |> printfn "%s"


let demo03 () = 
    text "Hello" ^+^ text "world!"
        |> testRenderText 80


let test04 () = 
    let m1 : Markdown = 
        unorderedList [ markdownText <| text "Hello"; markdownText <| text "world!" ]
    testRender 120 m1



