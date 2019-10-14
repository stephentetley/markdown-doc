// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace MarkdownDoc.Markdown


module InlineHtml = 
    
    open MarkdownDoc

    // This module is to be fleshed out.
    // A html_ prefix makes sense.

    type HtmlAttr = 
        internal | HtmlAttr of name : string * value : string

        member v.Attribute
            with get () : string = 
                let (HtmlAttr(name,value)) = v 
                sprintf "%s=\"%s\"" name value


    /// Memo - adding html attributes should be used sparingly.
    /// Obviously favour Markdown text combinators when one is 
    /// available for the text effect you want.
    let htmlAttr (attrName : string) (attrValue : string) : HtmlAttr =
        HtmlAttr(attrName, attrValue)

    type HtmlAttrs = HtmlAttr list

    type StyleDecl = 
        internal | StyleDecl of property : string * value : string

        member v.Style
            with get () : string = 
                let (StyleDecl(property,value)) = v 
                sprintf "%s:%s;" property value

    /// Memo - adding styling should be used sparingly.
    /// Obviously favour Markdown text combinators when one is 
    /// available for the text style you want.
    let styleDecl (property : string) (value : string) : StyleDecl = 
        StyleDecl(property, value)


    type StyleDecls = StyleDecl list


    let htmlElement (name : string) (attrs : HtmlAttrs) (body : Text) = 
        // Note - avoid angleBrackets as it inhibits good line breaking.
        let startTag = 
            match attrs |> List.map (fun x -> x.Attribute) with
            | [] -> sprintf "<%s>" name
            | xs -> sprintf "<%s %s>" name (String.concat " " xs)
        let endTag = sprintf "</%s>" name
        rawtext startTag ^^ body ^^ rawtext endTag


    /// ``<a id="anchorName">This is an anchor</a>``
    let htmlAnchorId (name : string) (body : Text) : Text = 
        htmlElement "a" [ htmlAttr "id" name ] body


    /// ``<a id="anchorName" attr1="value1" ...>This is an anchor</a>``
    /// Attrs suffix indicates this is the extended version of htmlAnchorId
    let htmlAnchorIdAttrs (name : string) (attrs : HtmlAttrs) (body : Text) : Text = 
        htmlElement "a" (HtmlAttr("id", name) :: attrs)  body


    /// ``<span>The text body...</span>``
    let htmlSpan (attrs : HtmlAttrs) (body : Text) : Text = 
        htmlElement "span" attrs body

        
    /// Title common appears as a tooltip.        
    let attrTitle (title : string) : HtmlAttr = 
        htmlAttr "title" title



    // ************************************************************************
    // Style declarations

    
    
    /// Typically for arbitrary colours. Obviously favour Markdown
    /// text combinators for text styles.
    let attrStyle (decls : StyleDecls) : HtmlAttr = 
        let body = decls |> List.map (fun x -> x.Style) |> String.concat ""
        htmlAttr "style"  body

    /// "background-color" style declaration
    let backgroundColor (value : string) : StyleDecl = 
        styleDecl "background-color" value


    /// "color" style declaration - aka text-color
    let color (value : string) : StyleDecl = 
        styleDecl "color" value

    /// "border" style declaration.
    /// Width is typically px e.g. "2px"
    let border (width : string option) 
               (style : string) 
               (color : string option) : StyleDecl = 
        let props = List.choose id [width; Some style; color]
        styleDecl "border" (String.concat "" props)

    /// "border-style" style declaration, e.g.
    /// "none" | "hidden" | "dotted" | "dashed" | "solid" |
    /// "double" | "groove" | "ridge" | ...
    let borderStyle (value : string) : StyleDecl = 
        styleDecl "border-style" value


    /// "border-color" style declaration
    let borderColor (value : string) : StyleDecl = 
        styleDecl "border-color" value

    /// "border-width" style declaration, typically [0-9]px or
    /// "thin" | "medium" | "thick" | ...
    let borderWidth (value : string) : StyleDecl = 
        styleDecl "border-width" value

    /// "border-radius" style declaration, typically in px
    /// Supply a single radius to round all four corners equally.
    let borderRadius (values : string list) : StyleDecl = 
        styleDecl "border-radius" (String.concat " " values)