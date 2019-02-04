// Copyright (c) Stephen Tetley 2018
// License: BSD 3 Clause

#r "netstandard"

open System.Text

#load "..\src\MarkdownDoc\Internal\Common.fs"
#load "..\src\MarkdownDoc\Internal\Syntax.fs"

open MarkdownDoc.Internal.Common
open MarkdownDoc.Internal.Syntax




let demo01 () =
    let text = HCatText (String "Hello", HCatText  (String " ", String "World!"))
    renderMdText text





let demo02 () =
    let text = HCatText (String "Hello", HCatText  (String " ", String "World!"))
    let olist = OrderedList [Text text; Text text; Text text]
    let ulist = UnorderedList [olist; Text text; Text text]
    renderMdPara ulist |> printfn "%s"





let demo03 () =
    let text = HCatText (String "Hello", HCatText  (String " ", String "World!"))
    let header = Paragraph (Text (String "# Title"))
    let olist = OrderedList [Text text; Text text; Text text]
    let ulist = UnorderedList [olist; Text text; Text text]
    let para1 = Paragraph ulist
    let document = VCatDoc(header, para1)
    renderMdDoc document |> printfn "%s"