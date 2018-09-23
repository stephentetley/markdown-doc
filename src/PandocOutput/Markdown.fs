// Copyright (c) Stephen Tetley 2018
// License: BSD 3 Clause

namespace PandocOutput

/// Design issue
/// Whitespace matters in Pandoc.
/// How explicitly should we treat it in this library?
/// Do we help the user (making the implementation complicated) or keep 
/// the implementation simple relying on the user to do the right thin?




open PandocOutput.Internal.FormatCombinators

[<AutoOpen>]
module Markdown = 


    type Alignment = AlignDefault | AlignLeft | AlignCenter | AlignRight

    
    type ColumnSpec = 
        { Width: int
          Alignment: Alignment }


    type System.Text.StringBuilder with
        member v.AppendLines(lines:string list) : unit = 
            List.iter (fun s -> v.AppendLine(s) |> ignore) lines

    /// We might change this to help process tables, blockquotes etc.
    type Markdown = Doc



    let render = PandocOutput.Internal.FormatCombinators.render
    let testRender = PandocOutput.Internal.FormatCombinators.testRender

    let inline private character (ch:char) : Markdown = formatChar ch
    let inline private mdstring (str:string) : Markdown = formatString str

    let plaintext (text:string) : Markdown = 
        mdstring text


    /// TODO - we should be careful about re-exports from FormatCombinators.
    /// We might want to change the type of ``Markdown``.
    let (+++) = PandocOutput.Internal.FormatCombinators.(+++)
    let (@@@) = PandocOutput.Internal.FormatCombinators.(@@@)
    let backslash = PandocOutput.Internal.FormatCombinators.backslash

    let h1 (source:Markdown) : Markdown = 
        character '#' +^+ source

    let h2 (source:Markdown) : Markdown = 
        mdstring "##" +^+ source

    let h3 (source:Markdown) : Markdown = 
        mdstring "###" +^+ source

    let h4 (source:Markdown) : Markdown = 
        mdstring "####" +^+ source

    let h5 (source:Markdown) : Markdown = 
        mdstring "#####" +^+ source

    let h6 (source:Markdown) : Markdown = 
        mdstring "######" +^+ source

    /// Emphasis
    let asterisks (source:Markdown) : Markdown = 
        enclose (character '*') (character '*') source

    /// Emphasis
    let underscores (source:Markdown) : Markdown = 
        enclose (character '_') (character '_') source

    /// Strong emphasis
    let doubleAsterisks (source:Markdown) : Markdown = 
        enclose (mdstring "**") (mdstring "**") source

    /// Strong emphasis
    let doubleUnderscores (source:Markdown) : Markdown = 
        enclose (mdstring "__") (mdstring "__") source


    let strikethrough (source:Markdown) : Markdown = 
        enclose (mdstring "~~") (mdstring "~~") source

    let raw (body:string) : Markdown = 
        mdstring "```" @@@ plaintext body @@@ mdstring "```"

    let raw2 (identifier:string) (body:string) : Markdown = 
        (mdstring "```"  +++ mdstring identifier) @@@ plaintext body @@@ mdstring "```"

    let docxPagebreak : Markdown = 
        let source : string = 
            [ "<w:p>"
            ; "  <w:r>"
            ; "    <w:br w:type=\"page\"/>"
            ; "  </w:r>"
            ; "</w:p>"
            ] |> String.concat "\n" 
        raw2 "{=openxml}" source


    let unordList (items:Markdown list) : Markdown = 
        vcat <| List.map (fun (doc:Markdown) -> character '*' +^+ doc) items

    /// Each line is prefixed with "> "
    let blockquote (source:Markdown) = prefix "> " source

    let comment (body:string) = empty

    /// Indent for a code block (4 spaces)
    let codeIndent : Markdown = mdstring "    "

    /// Indent for a code block (asterisk and 3 spaces)
    let unordListIndent : Markdown = mdstring "*   "