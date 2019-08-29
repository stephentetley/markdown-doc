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
        | StyleDecl of name : string * value : string

        member v.Style
            with get () : string = 
                let (StyleDecl(name,value)) = v 
                sprintf "%s:%s;" name value

    type StyleDecls = StyleDecl list


    let htmlElement (name : string) (attrs : HtmlAttrs) (body : Text) = 
        let startTag = 
            match attrs |> List.map (fun x -> x.Text) with
            | [] -> rawtext name |> angleBrackets
            | xs -> rawtext name ^+^ hsep xs |> angleBrackets
        let endTag = angleBrackets (rawtext <| sprintf "/%s" name)
        startTag ^^ body ^^ endTag

    /// ``<a id="anchorName">This is an anchor</a>``
    let htmlIdAnchor (name : string) (body : Text) : Text = 
        htmlElement name [] body


    /// ``<span >The text body...</a>``
    /// (Not useful without attributes...
    let htmlSpan (name : string) (attrs : HtmlAttrs) (body : Text) : Text = 
        htmlElement name attrs body


    let attrStyle (decls : StyleDecls) : HtmlAttr = 
        let body = decls |> List.map (fun x -> x.Style) |> String.concat ""
        HtmlAttr("style", body)