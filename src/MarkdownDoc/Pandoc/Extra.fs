// Copyright (c) Stephen Tetley 2018,2019
// License: BSD 3 Clause

namespace MarkdownDoc.Pandoc


[<AutoOpen>]
module Extra = 

    open MarkdownDoc
    open MarkdownDoc.Internal

    // TODO - once we have fixed rawtext

    //type Attributes =
    //    private | EmptyAttr
    //            | AttrText of string
    //            | HCat of Attributes * Attributes
    //    static member empty : Attributes = EmptyAttr

    //let private render (attributes:Attributes) : Text = 
    //    let rec work (attrs:Attributes) (cont : Text -> Text) : unit = 
    //        match attrs with
    //        | Empty -> cont Text.empty
    //        | Text(s) -> 
    //            cont (plaintext s)
    //        | Cat(x,y) -> 
    //            work x (fun _ ->
    //            work y cont)

    //    work x (fun t -> hgroup t)

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
            Syntax.table columnSpecs (Option.map makeRow headers) rows

    /// Produces '{=format}'
    let rawAttribute (format:string) : Text = 
        hgroup (braces (character '=' ^^ text format))

    /// Produces '`content`{=format}'            
    let inlineRaw (content:Text) (format:string) = 
        enclose (text "`") (text "`") content ^^ rawAttribute format

    /// TODO - should the equals sign be implicit?
    let rawCode (format:string) (codeSource:Text) : ParaElement = 
        let line1 = hgroup (backticks3 ^^ rawAttribute format)
        paraText line1 ^&^ paraText codeSource ^&^ paraText backticks3


    let openxmlPagebreak : Markdown = 
        let block = 
            [ "<w:p>"
            ; "  <w:r>"
            ; "    <w:br w:type=\"page\"/>"
            ; "  </w:r>"
            ; "</w:p>"
            ]
        markdown (rawCode "openxml" <| rawlines block)

    /// Strikeout the enclosed text.
    /// ~~deleted text~~
    let strikeout (source:Text) : Text = 
        enclose (text "~~") (text "~~") source

    /// Note - spaces in the superscript are escaped.
    /// The escaping relaces ' ' with "\ ".
    let superscript (source:string) : Text = 
        enclose (character '^') (character '^') 
                (text <| Common.escapeSpaces source)

    /// Note - spaces in the subscript are escaped.
    /// The escaping relaces ' ' with "\ ".
    let subscript (source:string) : Text = 
        enclose (character '~') (character '~') 
                (text <| Common.escapeSpaces source)

