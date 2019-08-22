// Copyright (c) Stephen Tetley 2018,2019
// License: BSD 3 Clause

#r "netstandard"
open System.IO

#I @"C:\Users\stephen\.nuget\packages\slformat\1.0.2-alpha-20190721\lib\netstandard2.0"
#r "SLFormat.dll"
open SLFormat.CommandOptions

#load "..\src\MarkdownDoc\Internal\Common.fs"
#load "..\src\MarkdownDoc\Internal\GridTable.fs"
#load "..\src\MarkdownDoc\Internal\SimpleDoc.fs"
#load "..\src\MarkdownDoc\Internal\Doc.fs"
#load "..\src\MarkdownDoc\Markdown\Text.fs"
#load "..\src\MarkdownDoc\Markdown\Block.fs"
#load "..\src\MarkdownDoc\Markdown\Table.fs"
#load "..\src\MarkdownDoc\Markdown\InlineHtml.fs"
#load "..\src\MarkdownDoc\Pandoc\Extra.fs"
#load "..\src\MarkdownDoc\Pandoc\Invoke.fs"

open MarkdownDoc.Markdown
open MarkdownDoc.Pandoc

let testDoc : Markdown = 
         h1 (text "Markdown Doc")
    ^!!^ markdownText (text "Code blocks are a line prefixing transformation (prefix each line by 4 spaces).")


let htmlOptions : PandocOptions = 
    pandocDefaults 
        |> addOption (argument "--self-contained") 
        |> addOption (argument "--highlight-style" &= "tango") 


let test01 () : Result<int, string>  =
    let workingDirectory = Path.Combine(__SOURCE_DIRECTORY__, @"../output/")
    let outputDocxName = "TestDoc.docx"
    let outputHtmlName = "TestDoc.html"
    let mdFileName = Path.Combine(workingDirectory, "TestDoc.md")
    let stylesDoc = @"../notes/include/custom-reference1.docx" 
    writeMarkdown 120 testDoc mdFileName
    runPandocDocx true workingDirectory mdFileName outputDocxName (Some stylesDoc) pandocDefaults |> ignore

    runPandocHtml5 true workingDirectory mdFileName outputHtmlName (Some "Test Doc") htmlOptions

