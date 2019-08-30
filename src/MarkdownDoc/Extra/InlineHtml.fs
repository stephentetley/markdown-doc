// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace MarkdownDoc.Markdown


module InlineHtml = 
    
    open MarkdownDoc

    // This module is to be fleshed out.
    // A html_ prefix makes sense.

    type HtmlAttr = 
        internal | HtmlAttr of name : string * value : string

        member v.Text
            with get () : Text = 
                let (HtmlAttr(name,value)) = v 
                rawtext name ^^ character '=' ^^ doubleQuotes (rawtext value) 
                    |> hgroup

    type HtmlAttrs = HtmlAttr list

    type StyleDecl = 
        internal | StyleDecl of property : string * value : string

        member v.Style
            with get () : string = 
                let (StyleDecl(property,value)) = v 
                sprintf "%s:%s;" property value

    type StyleDecls = StyleDecl list


    let htmlElement (name : string) (attrs : HtmlAttrs) (body : Text) = 
        // Note - avoid angleBrackets as it inhibits good line breaking.
        let startTag = 
            match attrs |> List.map (fun x -> x.Text) with
            | [] -> rawtext <| sprintf "<%s>" name
            | xs -> rawtext (sprintf "<%s" name) ^+^ hsep xs ^^ rawchar '>'
        let endTag = rawtext <| sprintf "</%s>" name
        startTag ^^ body ^^ endTag


    /// ``<a id="anchorName">This is an anchor</a>``
    let htmlAnchorId (name : string) (body : Text) : Text = 
        htmlElement "a" [ HtmlAttr("id", name) ] body


    /// ``<span >The text body...</a>``
    let htmlSpan (attrs : HtmlAttrs) (body : Text) : Text = 
        htmlElement "span" attrs body


    /// Memo - adding html attributes should be used sparingly.
    /// Obviously favour Markdown text combinators when one is 
    /// available for the text effect you want.
    let htmlAttr (attrName : string) (attrValue : string) : HtmlAttr =
        HtmlAttr(attrName, attrValue)

    /// Typically for arbitrary colours. Obviously favour Markdown
    /// text combinators for text styles.
    let attrStyle (decls : StyleDecls) : HtmlAttr = 
        let body = decls |> List.map (fun x -> x.Style) |> String.concat ""
        htmlAttr "style"  body
        
    /// Title common appears as a tooltip.        
    let attrTitle (title : string) : HtmlAttr = 
        htmlAttr "title" title



    // ************************************************************************
    // Style declarations

    let styleDecl (property : string) (value : string) : StyleDecl = 
        StyleDecl(property, value)

    let backgroundColor (value : string) : StyleDecl = 
        styleDecl "background-color" value



