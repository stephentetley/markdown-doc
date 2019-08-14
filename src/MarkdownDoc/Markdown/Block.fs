// Copyright (c) Stephen Tetley 2018,2019
// License: BSD 3 Clause

namespace MarkdownDoc.Markdown



[<AutoOpen>]
module TBlockext = 

    open System.IO

    open MarkdownDoc.Internal
    open MarkdownDoc

    type Indent = SimpleDoc.Indent


    // ************************************************************************
    // Block elements


    /// Paragraph assembles Text
    type Markdown = Doc.MdBlock

    let emptyMarkdown : Markdown = Doc.EmptyBlock

    let markdownText (text : Text) : Markdown = 
        Doc.TextBlock text

    let markdownLines (lines : Text list) : Markdown = 
        lines |> textlines |> Doc.TextBlock
    
    /// Vertical concat.
    let ( ^!!^ ) (doc1 : Markdown) (doc2 : Markdown) : Markdown = 
        Doc.VCatBlock(doc1, doc2)


    //let unorderedList (elements:Block list) : Block = 
    //    Syntax.uList elements

    //let orderedList (elements:Block list) : Block = 
    //    Syntax.oList elements

    //let private defReference (identifier:string) 
    //                         (path:string) 
    //                         (title:string option) : Text = 
    //    let body1 = 
    //        squareBrackets (text identifier) ^^ colon ^+^ text (Common.replaceBackslashes path)
    //    match title with
    //    | None -> body1
    //    | Some ss -> body1 ^+^ doubleQuotes (text ss)

    ///// [id]: path/to 
    ///// [id]: path/to "Title"
    //let defLinkReference (identifier:string) 
    //                     (path:string) 
    //                     (title:string option) : ParaElement = 
    //    paraText (hgroup (defReference identifier path title))


    ///// ![id]: path/to 
    ///// ![id]: path/to "Title"
    //let defImageReference (identifier:string) 
    //                      (path:string) 
    //                      (title:string option) : ParaElement = 
    //     paraText (hgroup (bang ^^ defReference identifier path title))




    let inline private atxHeader (hashes:string) (content:Text) : Markdown = 
        markdownText (hgroup (text hashes ^+^ content))

    /// Atx style header H1
    let h1 (content:Text) : Markdown = atxHeader "#" content
    
    /// Atx style header H2
    let h2 (content:Text) : Markdown = atxHeader "##" content

    /// Atx style header H3
    let h3 (content:Text) : Markdown = atxHeader "###" content

    /// Atx style header H4
    let h4 (content:Text) : Markdown = atxHeader "####" content

    /// Atx style header H5
    let h5 (content:Text) : Markdown = atxHeader "#####" content

    /// Atx style header H6
    let h6 (content:Text) : Markdown = atxHeader "######" content

    

    /// Code block indents the paragraph with four spaces.
    let codeBlock (body : Markdown) : Markdown = 
        Doc.Block (Indent.Uniform 4, body)



    /// Concatenate a list of Markdown fragments.        
    let concatMarkdown (elements : Markdown list) : Markdown = 
        match elements with
        | [] -> emptyMarkdown
        | d1 :: rest -> List.fold (^!!^) d1 rest

    /// nbsp
    let nbsp : Markdown = markdownText (entity "nbsp")


    /// Page break / Horizontal Rule
    /// Printed as five asterisks.
    let horizontalRule : Markdown = markdownText (text "*****")


    let renderMarkdown (lineWidth : int) (source : Markdown) : string = 
        Doc.blockToSimpleDoc source
            |> SimpleDoc.renderSimpleDoc lineWidth

    /// Print the Text to the console.
    let testRenderText (lineWidth : int) (source:Text) : unit = 
        renderMarkdown lineWidth (markdownText source) 
            |> printfn  "----------\n%s\n----------\n"


