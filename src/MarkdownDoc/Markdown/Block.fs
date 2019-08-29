// Copyright (c) Stephen Tetley 2018,2019
// License: BSD 3 Clause

namespace MarkdownDoc.Markdown



[<AutoOpen>]
module Block = 

    open System.IO

    open MarkdownDoc.Internal
    open MarkdownDoc

    type Indent = SimpleDoc.Indent


    // ************************************************************************
    // Block elements


    /// Paragraph assembles Text
    type Markdown = Doc.MdBlock

    let emptyMarkdown : Markdown = Doc.EmptyBlock

    let blankLine : Markdown = Doc.BlankLine
    
    
    let markdownText (text : Text) : Markdown = 
        Doc.TextBlock text

    let markdownLines (lines : Text list) : Markdown = 
        lines |> textlines |> Doc.TextBlock
    
    /// Vertical concat - no blank line between.
    let ( ^!^ ) (doc1 : Markdown) (doc2 : Markdown) : Markdown = 
        Doc.VCatBlock(doc1, doc2)

    /// Vertical concat with a blank line between.
    let ( ^!!^ ) (doc1 : Markdown) (doc2 : Markdown) : Markdown = 
        doc1 ^!^ blankLine ^!^ doc2


    let vcat (blocks : Markdown list) : Markdown = 
        match blocks with
        | [] -> emptyMarkdown
        | b1 :: rest -> List.fold (^!^) b1 rest

    let vsep (docs : Markdown list) : Markdown = 
        match docs with 
        | [] -> emptyMarkdown
        | [d1] -> d1
        | d1 :: rest -> List.fold (fun ac d -> ac ^!!^ d) d1 rest


    let simpleIndent (prefix : string)  (body : Markdown) : Markdown = 
        Doc.Block(Indent.Uniform prefix, body)

    let hangingIndent (firstLinePrefix : string) 
                        (remainingLinesPrefix : string)
                        (body : Markdown) : Markdown = 
        Doc.Block(Indent.Hanging (firstLinePrefix, remainingLinesPrefix), body)


    /// An unordered list with a custom item marker 
    let internal customUnorderedList (marker : char) 
                                     (elements : Markdown list) : Markdown = 
        let indent1 = sprintf "%c   " marker
        let indent2 = "    "
        let listItem (d1 : Markdown) = hangingIndent indent1 indent2 d1
        elements |> List.map listItem |> vcat


    /// ERROR - Does not indent forlong texts that have been split across lines
    /// Draw an unordered list with star as the item marker.
    let unorderedList (elements : Markdown list) : Markdown = 
        customUnorderedList '*' elements

    /// ERROR - Does not indent forlong texts that have been split across lines
    /// Draw an unordered list with plus as the item marker.
    let unorderedListWithPlus (elements : Markdown list) : Markdown = 
        customUnorderedList '+' elements
    
    /// ERROR - Does not indent forlong texts that have been split across lines
    /// Draw an unordered list with minus (dash) as the item marker.
    let unorderedListWithMinus (elements : Markdown list) : Markdown = 
        customUnorderedList '-' elements

    /// ERROR - Does not indent forlong texts that have been split across lines
    let orderedList (elements : Markdown list) : Markdown = 
        let size = 4 + elements.Length.ToString().Length
        let fill (width : int) (str : string) : string = 
            str.PadRight(totalWidth = width, paddingChar = ' ')
        let (items, _) = 
            List.mapFold (fun (ix : int) (d1 : Markdown) -> 
                            let a = fill size (sprintf "%i." ix)
                            let b = String.replicate size " "
                            let d2 = hangingIndent a b d1
                            (d2, ix + 1))
                        1
                        elements
        vcat items

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
        Doc.Block (Indent.Uniform "    ", body)




    /// nbsp
    let nbsp : Markdown = markdownText (entity "nbsp")


    /// Page break / Horizontal Rule
    /// Printed as five asterisks.
    let horizontalRule : Markdown = markdownText (text "*****")


    let renderMarkdown (lineWidth : int) (source : Markdown) : string = 
        Doc.blockToSimpleDoc source
            |> SimpleDoc.renderSimpleDoc lineWidth


    let writeMarkdown (lineWidth : int) (source : Markdown) (fileName : string) : unit = 
        let output = renderMarkdown lineWidth source
        File.WriteAllText(path = fileName, contents = output)

    /// Print the Text to the console.
    let testRenderText (lineWidth : int) (source : Text) : unit = 
        renderMarkdown lineWidth (markdownText source) 
            |> printfn  "----------\n%s\n----------\n"


    /// Print the Markdown to the console.
    let testRender (lineWidth : int) (source : Markdown) : unit = 
        renderMarkdown lineWidth source
            |> printfn  "----------\n%s\n----------\n"