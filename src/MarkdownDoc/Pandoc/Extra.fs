// Copyright (c) Stephen Tetley 2018,2019
// License: BSD 3 Clause

namespace MarkdownDoc.Pandoc


[<AutoOpen>]
module Extra = 

    open MarkdownDoc
    open MarkdownDoc.Internal.Common

    let inlineRaw (format:string) (content:Text) = 
        enclose (text "`") (text "`") content ^^ text (sprintf "={%s}" format)

    /// TODO - should the equals sign be implicit?
    let rawCode (rawAttr:string) (codeSource:string) : Markdown = 
        let line1 = backticks3 ^^ braces (equals ^^ plaintext rawAttr)
        let textlines = plainlines <| toLines codeSource
        unboundedTile <| line1 ^&^ textlines ^&^ backticks3


    let openxmlPagebreak : Markdown = 
        let block = 
            [ "<w:p>"
            ; "  <w:r>"
            ; "    <w:br w:type=\"page\"/>"
            ; "  </w:r>"
            ; "</w:p>"
            ]
        rawCode "openxml" <| fromLines block