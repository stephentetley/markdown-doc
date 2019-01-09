// Copyright (c) Stephen Tetley 2018,2019
// License: BSD 3 Clause

namespace MarkdownDoc.Pandoc.Extra


[<AutoOpen>]
module Extra = 

    open MarkdownDoc
    open MarkdownDoc.Internal

    let inlineRaw (format:string) (content:Text) = 
        enclose (text "`") (text "`") content ^^ text (sprintf "={%s}" format)

    /// TODO - this can be made to use Text combinators
    let rawCode (format:string) (codeSource:string) : Markdown = 
        let line1 = sprintf "```{=%s}" format
        let line2 = "```"
        let textlines = toLines codeSource
        preformatted << List.map rawtext <| (line1 :: (textlines @ [line2]))

    let openxmlPagebreak : Markdown = 
        let block = 
            [ "<w:p>"
            ; "  <w:r>"
            ; "    <w:br w:type=\"page\"/>"
            ; "  </w:r>"
            ; "</w:p>"
            ]
        rawCode "openxml" <| fromLines block