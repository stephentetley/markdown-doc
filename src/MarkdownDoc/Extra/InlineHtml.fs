// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace MarkdownDoc.Markdown


module InlineHtml = 
    
    open MarkdownDoc

    // This module is to be fleshed out.
    // A html_ prefix makes sense.

    type HtmlAttr = 
        | HtmlAttr of name : string * value : string

        member v.Text
            with get () : Text = 
                let (HtmlAttr(name,value)) = v 
                rawtext name ^^ character '=' ^^ doubleQuotes (rawtext value) 
                    |> hgroup

    type HtmlAttrs = HtmlAttr list

    type StyleDecl = 
        | StyleDecl of property : string * value : string

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


    let attrStyle (decls : StyleDecls) : HtmlAttr = 
        let body = decls |> List.map (fun x -> x.Style) |> String.concat ""
        HtmlAttr("style", body)



    // ************************************************************************
    // Style declarations


    let backgroundColor (value : string) : StyleDecl = 
        StyleDecl("background-color", value)

