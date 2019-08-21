// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace MarkdownDoc.Markdown

[<AutoOpen>]
module InlineHtml = 
    
    open MarkdownDoc

    // This module is to be fleshed out.
    // A html_ prefix makes sense.


    /// ``<a id="anchorName">This is an anchor</a>``
    let htmlIdAnchor (name : string) (body : Text) : Text = 
        rawtext (sprintf "<a id=\"%s\">" name) ^^ body ^^ rawtext "</a>"
