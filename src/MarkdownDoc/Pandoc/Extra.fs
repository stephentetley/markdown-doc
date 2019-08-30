// Copyright (c) Stephen Tetley 2018,2019
// License: BSD 3 Clause

namespace MarkdownDoc.Pandoc


[<AutoOpen>]
module Extra = 

    open MarkdownDoc.Internal
    open MarkdownDoc.Internal.Doc
    open MarkdownDoc.Markdown
    

    [<Struct>]
    type PandocAttribute = 
        | PandocAttribute of Text
        member v.AttributeText 
            with get () : Text = let (PandocAttribute t) = v in t

    /// Renders as an hgroup inside braces, space separated.
    let private renderAttrs (attrs:PandocAttribute list) : Text = 
        attrs 
            |> List.map (fun x -> x.AttributeText)
            |> hsep
            |> braces
            |> hgroup

    /// Note - the function prefixes the name with a hash (#).
    /// You don't have to.
    let identifier (name:string) : PandocAttribute = 
        PandocAttribute <| rawtext (sprintf "#%s" name)

    /// Note - the function prefixes the name with a dot (.).
    /// You don't have to.
    let selector (name:string) : PandocAttribute = 
        PandocAttribute <| rawtext (sprintf ".%s" name)

    let keyValue (key:string) (value:Text) : PandocAttribute = 
        PandocAttribute (rawtext key ^^ equalsSign ^^ value)

    /// Produces '=haskell' where formatName ="haskell"
    let rawAttribute (formatName:string) : PandocAttribute = 
        PandocAttribute (equalsSign ^^ rawtext formatName)



    /// Grid Table        
    /// Table printed in the `grid_table` style.
    


    let gridTable (table : Table) : Markdown = 
       MdBlock.TableBlock(table.ColumnSpecs, table.ColumnHeadings, table.Rows)


    /// Produces '`content`{=format}'            
    let inlineRaw (content:Text) (format:string) = 
        enclose (text "`") (text "`") content ^^ renderAttrs [rawAttribute format]

    /// Multiline raw code with format attribute
    /// ```{=format}
    /// _body_
    /// ```
    let rawCode (format : string) (codeSource : Markdown) : Markdown = 
        let line1 = hgroup (backticks3 ^^ renderAttrs [rawAttribute format])
        markdownText line1 ^!!^ codeSource ^!!^ markdownText backticks3

    /// ```{=openxml}
    /// <w:p>
    /// _etc_
    /// </w:p>
    /// ```
    let openxmlPagebreak : Markdown = 
        let block = 
            [ "<w:p>"
            ; "  <w:r>"
            ; "    <w:br w:type=\"page\"/>"
            ; "  </w:r>"
            ; "</w:p>"
            ]
        rawCode "openxml" << markdownText <| rawlines block

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


    /// Fenced code
    let fencedCode (fenceWidth : int) (body : Markdown) : Markdown = 
        let fence = replicated fenceWidth "~" |> markdownText
        fence ^!^ body ^!^ fence

    /// Line block - each line prefixed with `| `
    let lineBlock (body : Markdown) : Markdown = 
        simpleIndent "| " body

    /// A 'fancy list' - each element is prefixed with `#.  `
    let hashFancyList (elements : Markdown list) : Markdown = 
        let listItem (d1 : Markdown) = hangingIndent "#.  " "    " d1
        elements |> List.map listItem |> vcat

