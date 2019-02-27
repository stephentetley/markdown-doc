// Copyright (c) Stephen Tetley 2018,2019
// License: BSD 3 Clause

#r "netstandard"
#r "System.Xml.Linq.dll"

// Use FSharp.Data for CSV reading
#I @"C:\Users\stephen\.nuget\packages\FSharp.Data\3.0.0\lib\netstandard2.0"
#r @"FSharp.Data.dll"
open FSharp.Data

#I @"C:\Users\stephen\.nuget\packages\slformat\1.0.2-alpha-20190207\lib\netstandard2.0"
#r "SLFormat.dll"

#load "..\src\MarkdownDoc\Internal\Common.fs"
#load "..\src\MarkdownDoc\Internal\Syntax.fs"
#load "..\src\MarkdownDoc\Markdown.fs"
#load "..\src\MarkdownDoc\Pandoc\Invoke.fs"

open MarkdownDoc
open MarkdownDoc.Pandoc.Invoke

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


type Item = 
    { Uid: string 
      Name: string
      Worklist: string list }

let nbsp2 : Markdown = 
    markdownTile <| nbsp ^&^ nbsp

let logo : Markdown = 
    markdownTile (inlineImage "" @"include/YW-logo.jpg" None)

let title1 : Markdown = 
    h1 (text "T0975 - Event Duration Monitoring Phase 2 (EDM2)")
    

let title2 (sai:string) (name:string) : Markdown = 
    h2 (text sai ^+^ text name)



let contents (workItems:string list) : Markdown = 
    h3 (text "Contents") ^@^ markdown (unordList (List.map (paraText << text) workItems))

let documentControl : Markdown = 
    h3 (text "Document Control")

let makeDoc (item:Item) : Markdown = 
    concatMarkdown 
        <| [ logo
           ; nbsp2
           ; title1
           ; nbsp2
           ; title2 item.Uid item.Name
           ; nbsp2
           ; contents item.Worklist
           ]



let generateDocx (workingDirectory:string) (mdInputPath:string) (outputDocxName:string) : unit  =
    let stylesDoc = @"include/custom-reference1.docx" 
    runPandocDocx workingDirectory mdInputPath outputDocxName (Some stylesDoc) pandocDefaults

