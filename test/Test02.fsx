// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

#r "netstandard"

#I @"C:\Users\stephen\.nuget\packages\slformat\1.0.2-alpha-20190712\lib\netstandard2.0"
#r "SLFormat.dll"

#load "..\src\MarkdownDoc\Internal\Common.fs"
#load "..\src\MarkdownDoc\Internal\Syntax.fs"
#load "..\src\MarkdownDoc\Markdown\Markdown.fs"
#load "..\src\MarkdownDoc\Pandoc\Extra.fs"
#load "..\src\MarkdownDoc\Pandoc\Invoke.fs"

open MarkdownDoc.Markdown
open MarkdownDoc.Pandoc

// This test should indicate whether table rendering 
// is incorrectly reversing lines.

let columnSpecs : ColumnSpec list = 
    [  { Width = 20; Alignment = Alignment.AlignLeft }
    ;  { Width = 14; Alignment = Alignment.AlignLeft }
    ;  { Width = 16; Alignment = Alignment.AlignLeft }
    ;  { Width = 24; Alignment = Alignment.AlignLeft }
    ]


/// | 1.0 | S Tetley | 18/10/2018 | For EDMS |
let controlTable (author:string) : Markdown = 

    let nowstring = System.DateTime.Now.ToShortDateString()

    let makeHeaderCell (s:string) : ParaElement = 
        text s |> doubleAsterisks |> paraText

    let makeCell (s:string) : ParaElement = text s |> paraText

    let headers = 
        List.map makeHeaderCell ["Revision"; "Prepared By"; "Date"; "Comments"]
    let row1 = 
        List.map makeCell ["1.0"; author; nowstring; "For EDMS"]
    let row2 = [ParaElement.empty; ParaElement.empty; ParaElement.empty; ParaElement.empty]
    gridTable columnSpecs (Some headers) [row1; row2] 

let demo01 () = 
    controlTable "S Tetley" |> testRender

