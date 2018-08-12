// Copyright (c) Stephen Tetley 2018
// License: BSD 3 Clause

namespace PandocOutput



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

    let inline private mdchar (ch:char) : Markdown = formatChar ch
    let inline private mdstring (str:string) : Markdown = formatString str

    let plaintext (text:string) : Markdown = 
        mdstring text


    /// TODO - we should be careful about rexports from FormatCombinators.
    /// We might want to change the type of ``Markdown``.
    let (+++) = PandocOutput.Internal.FormatCombinators.(+++)
    let backslash = PandocOutput.Internal.FormatCombinators.backslash

    let h1 (source:Markdown) : Markdown = 
        mdchar '#' +^+ source

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
        enclose (mdchar '*') (mdchar '*') source

    /// Emphasis
    let underscores (source:Markdown) : Markdown = 
        enclose (mdchar '_') (mdchar '_') source

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
        vcat <| List.map (fun (doc:Markdown) -> mdchar '*' +^+ doc) items

