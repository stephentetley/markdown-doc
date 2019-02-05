// Copyright (c) Stephen Tetley 2018
// License: BSD 3 Clause

#r "netstandard"

open System.Text

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