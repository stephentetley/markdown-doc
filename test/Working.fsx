// Copyright (c) Stephen Tetley 2018,2019
// License: BSD 3 Clause

#r "netstandard"
open System.Text

#I @"C:\Users\stephen\.nuget\packages\slformat\1.0.2-alpha-20190207\lib\netstandard2.0"
#r "SLFormat.dll"

#load "..\src\MarkdownDoc\Internal\Common.fs"
#load "..\src\MarkdownDoc\Internal\Syntax.fs"


open MarkdownDoc.Internal.Common
open MarkdownDoc.Internal.Syntax


let demo01 () =
    let text = HCatText (Text "Hello", HCatText  (Text " ", Text "World!"))
    renderMdText text

let demo02 () =
    let text = HCatText (Text "Hello", HCatText  (Text " ", Text "World!"))
    let olist = OrderedList [ParaText text; ParaText text; ParaText text]
    let ulist = UnorderedList [olist; ParaText text; ParaText text]
    renderMdPara ulist |> printfn "%s"


let demo03 () =
    let text = HCatText (Text "Hello", HCatText  (Text " ", Text "World!"))
    let header = Paragraph (ParaText (Text "# Title"))
    let olist = OrderedList [ParaText text; ParaText text; ParaText text]
    let ulist = UnorderedList [olist; ParaText text; ParaText text]
    let para1 = Paragraph ulist
    let document = VCatDoc(header, para1)
    renderMdDoc document |> printfn "%s"

let test01 () = 
    let text = "one two  three   four"
    let splits = text.Split(' ')
    String.concat " " splits 

let test02 () = 
    raggedMap2 (sprintf "%c-%d") ['a';'b';'c';'d'] [1;2;3;4;5] 

