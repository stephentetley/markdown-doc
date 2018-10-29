// Copyright (c) Stephen Tetley 2018
// License: BSD 3 Clause

namespace MarkdownDoc.Pandoc

open MarkdownDoc

[<AutoOpen>]
module PandocExtras = 


    let rawblock (formatName:string) (textlines:string list) : Markdown = 
        let line1 = sprintf"```{=%s}" formatName
        let line2 = "```"
        preformatted << List.map text <| (line1 :: (textlines @ [line2]))

    let openXmlPagebreak : Markdown = 
        rawblock "openxml" 
            [ "<w:p>"
            ; "  <w:r>"
            ; "    <w:br w:type=\"page\"/>"
            ; "  </w:r>"
            ; "</w:p>"
            ]