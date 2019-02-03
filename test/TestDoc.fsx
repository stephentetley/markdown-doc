// Copyright (c) Stephen Tetley 2018
// License: BSD 3 Clause

#r "netstandard"

open System.IO

#load "..\src\MarkdownDoc\Internal\Common.fs"
#load "..\src\MarkdownDoc\Internal\MarkdownText.fs"
#load "..\src\MarkdownDoc\Internal\MarkdownTile.fs"
#load "..\src\MarkdownDoc\Markdown.fs"
#load "..\src\MarkdownDoc\Pandoc\Extra.fs"
#load "..\src\MarkdownDoc\Pandoc\Invoke.fs"

open MarkdownDoc
open MarkdownDoc.Internal
open MarkdownDoc.Pandoc

let testDoc : Markdown = 
    h1 (text "Markdown Doc")


let test01 () : unit  =
    let workingDirectory = Path.Combine(__SOURCE_DIRECTORY__, @"../output/")
    let outputDocxName = "TestDoc.docx"
    let mdFileName = "TestDoc.md"
    let stylesDoc = @"../notes/custom-reference1.docx" 
    testDoc.Save(Path.Combine(workingDirectory, mdFileName))
    runPandocDocx workingDirectory mdFileName outputDocxName stylesDoc []
