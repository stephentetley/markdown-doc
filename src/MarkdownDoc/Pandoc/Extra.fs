// Copyright (c) Stephen Tetley 2018,2019
// License: BSD 3 Clause

namespace MarkdownDoc.Pandoc


[<AutoOpen>]
module Extra = 

    open MarkdownDoc
    open MarkdownDoc.Internal


    // TODO - we should reify attributes to help us compose them


    /// Grid Table        
    /// Table printed in the `grid_table` style.
    let gridTable (columnSpecs:ColumnSpec list) 
                  (headers: (ParaElement list) option)
                  (contents: (ParaElement list) list) : Markdown = 

        let makeCell (spec:ColumnSpec) (para:ParaElement) : Syntax.TableCell = 
            { Width = spec.Width
              Content = para }

        let makeRow (row:ParaElement list) : Syntax.TableRow = 
            Common.raggedMap2 makeCell columnSpecs row

        Markdown <| fun _ ->
            let rows = List.map makeRow contents
            Syntax.Table(columnSpecs, Option.map makeRow headers, rows)


    let inlineRaw (format:string) (content:Text) = 
        enclose (text "`") (text "`") content ^^ text (sprintf "={%s}" format)

    /// TODO - should the equals sign be implicit?
    let rawCode (rawAttr:string) (codeSource:Text) : ParaElement = 
        let line1 = hgroup (backticks3 ^^ braces (equals ^^ plaintext rawAttr))
        paraText line1 ^/^ paraText codeSource ^/^ paraText backticks3


    let openxmlPagebreak : Markdown = 
        let block = 
            [ "<w:p>"
            ; "  <w:r>"
            ; "    <w:br w:type=\"page\"/>"
            ; "  </w:r>"
            ; "</w:p>"
            ]
        markdown (rawCode "openxml" <| plainlines block)

    /// Strikeout the enclosed text.
    /// ~~deleted text~~
    let strikeout (source:Text) : Text = 
        enclose (text "~~") (text "~~") source

    /// Note - spaces in the superscript are escaped.
    let superscript (source:string) : Text = 
        enclose (character '^') (character '^') 
                (plaintext <| Common.escapeSpaces source)

    /// Note - spaces in the subscript are escaped.
    let subscript (source:string) : Text = 
        enclose (character '~') (character '~') 
                (plaintext <| Common.escapeSpaces source)

