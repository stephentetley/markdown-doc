// Copyright (c) Stephen Tetley 2018,2019
// License: BSD 3 Clause

namespace MarkdownDoc.Pandoc


[<AutoOpen>]
module Extra = 

    open MarkdownDoc
    open MarkdownDoc.Internal

    [<Struct>]
    type Attribute = 
        | Attribute of Text
        member x.Body 
            with get () : Text = match x with | Attribute(t) -> t

    /// Renders as an hgroup inside braces, space separated.
    let private renderAttrs (attrs:Attribute list) : Text = 
        attrs 
            |> List.map (fun x -> x.Body)
            |> hsep
            |> braces
            |> hgroup

    /// Note - the function prefixes the name with a hash (#).
    /// You don't have to.
    let identifier (name:string) : Attribute = 
        Attribute <| rawtext (sprintf "#%s" name)

    /// Note - the function prefixes the name with a dot (.).
    /// You don't have to.
    let selector (name:string) : Attribute = 
        Attribute <| rawtext (sprintf ".%s" name)

    let keyValue (key:string) (value:Text) : Attribute = 
        Attribute (rawtext key ^^ equalsSign ^^ value)

    /// Produces '=format'
    let rawAttribute (formatName:string) : Attribute = 
        Attribute (equalsSign ^^ rawtext formatName)

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



    /// Produces '`content`{=format}'            
    let inlineRaw (content:Text) (format:string) = 
        enclose (text "`") (text "`") content ^^ renderAttrs [rawAttribute format]

    /// Multiline raw code with format attribute
    let rawCode (format:string) (codeSource:Text) : ParaElement = 
        let line1 = hgroup (backticks3 ^^ renderAttrs [rawAttribute format])
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

