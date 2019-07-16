// Copyright (c) Stephen Tetley 2018,2019
// License: BSD 3 Clause

#r "netstandard"
open System.IO

#I @"C:\Users\stephen\.nuget\packages\slformat\1.0.2-alpha-20190712\lib\netstandard2.0"
#r "SLFormat.dll"

#load "..\src\MarkdownDoc\Internal\Common.fs"
#load "..\src\MarkdownDoc\Internal\Syntax.fs"
#load "..\src\MarkdownDoc\Markdown\Markdown.fs"
#load "..\src\MarkdownDoc\Markdown\Table.fs"
#load "..\src\MarkdownDoc\Pandoc\Extra.fs"
#load "..\src\MarkdownDoc\Pandoc\Invoke.fs"

open MarkdownDoc.Markdown
open MarkdownDoc.Pandoc

let testDoc : Markdown = 
         h1 (text "Markdown Doc")
    ^!!^ markdownText (text "Code blocks are a line prefixing transformation (prefix each line by 4 spaces).")


let test01 () : unit  =
    let workingDirectory = Path.Combine(__SOURCE_DIRECTORY__, @"../output/")
    let outputDocxName = "TestDoc.docx"
    let mdFileName = Path.Combine(workingDirectory, "TestDoc.md")
    let stylesDoc = @"../notes/include/custom-reference1.docx" 
    testDoc.Save( mdFileName)
    runPandocDocx workingDirectory mdFileName outputDocxName (Some stylesDoc) pandocDefaults
