// Copyright (c) Stephen Tetley 2018
// License: BSD 3 Clause

namespace MarkdownDoc.Pandoc.Extra


[<AutoOpen>]
module Extra = 

    open MarkdownDoc

    let inlineRaw (format:string) (content:Text) = 
        enclose (text "`") (text "`") content ^^ text (sprintf "={%s}" format)

    let rawCode (format:string) (textlines:string list) : Markdown = 
        let line1 = sprintf"```{=%s}" format
        let line2 = "```"
        preformatted << List.map rawtext <| (line1 :: (textlines @ [line2]))

    let openxmlPagebreak : Markdown = 
        rawCode "openxml" 
            [ "<w:p>"
            ; "  <w:r>"
            ; "    <w:br w:type=\"page\"/>"
            ; "  </w:r>"
            ; "</w:p>"
            ]