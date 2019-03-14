// Copyright (c) Stephen Tetley 2018,2019
// License: BSD 3 Clause

namespace MarkdownDoc

// Design issue
// Whitespace matters in Pandoc.
// With the different doc fragments type we should be handling
// it implicitly - check this assumption!


// VCat Note.
// The combinator symbols get heavier (^/^), (^&^), (^@^)
// as the text object the combine is larger.



[<AutoOpen>]
module Markdown = 

    open System.IO

    open MarkdownDoc.Internal
    
    type Alignment = Syntax.Alignment
    type ColumnSpec = Syntax.ColumnSpec


    /// Text is the type for 'body text'. 
    /// Sentences and markup smaller than a paragraph.
    type Text = Syntax.MdText

    /// The empty Text document.
    let emptyText : Text = Text.empty

    /// Bind the text into a unbreakable group
    let hgroup (text:Text) : Text = 
        Syntax.groupText text

    /// Build a Text item from a single char. 
    /// '&' and '<' will be escaped.
    let character (ch:char) : Text = 
        match ch with
        | '<' -> Syntax.rawText "&lt;"
        | '&' -> Syntax.rawText "&amp;"
        | _ -> Syntax.rawText <| ch.ToString()

    /// Build a Text item from a single char. 
    /// No escaping.
    let rawchar (ch:char) : Text = Syntax.rawText <| ch.ToString()
        

    /// Build a Text item from a string. 
    /// '&' and '<' will be escaped.
    /// No line splitting.
    let text (content:string) : Text = Syntax.escapedText content  
        
    /// Build a Text item from a string. 
    /// No escaping is performed, use this function with care.
    let rawtext (content:string) : Text = 
        Syntax.rawText content  

    /// Build a multiline Text item from a string. 
    /// No escaping is performed, use this function with care.
    let rawlines (contents:string list) : Text = 
        Syntax.belowTexts <| List.map rawtext contents


    /// Horizontal concat directly (no separating space)
    let ( ^^ ) (d1:Text) (d2:Text) : Text = 
        Syntax.beside d1 d2

    /// Horizontal concat with a separating space 
    let ( ^+^ ) (d1:Text) (d2:Text) : Text = 
        Syntax.besideSpace d1 d2

    /// Vertical concat.
    let ( ^/^ ) (d1:Text) (d2:Text) : Text = 
        Syntax.belowText d1 d2

    /// Build a text document from a list of Text lines.
    /// The lines are vertically concatenated.
    let textlines (lines:Text list) : Text = 
        Syntax.belowTexts lines

    /// Horizontal concatenate. No separating space.
    let hcat (items:Text list) : Text = 
        List.fold (^^) Text.empty items


    /// Horizontal concatenate with a separating space.
    let hsep (items:Text list) : Text = 
        List.fold (^+^) Text.empty items

    let bang : Text = character '!'
    let colon : Text = character ':'
    let space : Text = character ' '
    let equalsSign : Text = character '='

    let entity (name:string) : Text = rawtext <| sprintf "&%s;" name



    let copyright : Text = entity "copy"
    let registered : Text = entity "reg"
    let trademark : Text = entity "trade"

    let apostrophe :Text = entity "apos"
    let doublequote : Text = entity "quot"

    let lessthan : Text = entity "lt"
    let greaterthan : Text = entity "gt"
    let ampersand : Text = entity "amp"

    let cent : Text = entity "cent"
    let pound : Text = entity "pound"
    let yen : Text = entity "yen"
    let euro : Text = entity "euro"

    /// Print 3 backticks.
    let backticks3 : Text = rawtext "```"


    let enclose (left:Text) (right:Text) (d1:Text) : Text = 
        left ^^ d1 ^^ right


    let parens (source:Text) : Text = 
        enclose (rawchar '(') (rawchar ')') source

    let squareBrackets (source:Text) : Text = 
        enclose (rawchar '[') (rawchar ']') source

    /// Can be used for inlining links.
    let angleBrackets (source:Text) : Text = 
        enclose (rawchar '<') (rawchar '>') source

    /// Curly braces
    let braces (source:Text) : Text = 
        enclose (rawchar '{') (rawchar '}') source


    let singleQuotes (source:Text) : Text = 
        enclose (rawchar '\'') (rawchar '\'') source

    let doubleQuotes (source:Text) : Text = 
        enclose (rawchar '"') (rawchar '"') source

    /// Emphasis
    let asterisks (source:Text) : Text = 
        enclose (rawchar '*') (rawchar '*') source

    /// Emphasis
    let underscores (source:Text) : Text = 
        enclose (rawchar '_') (rawchar '_') source

    /// Strong emphasis
    let doubleAsterisks (source:Text) : Text = 
        enclose (rawtext "**") (rawtext "**") source

    /// Strong emphasis
    let doubleUnderscores (source:Text) : Text = 
        enclose (rawtext "__") (rawtext "__") source

    /// Backticks for inline code.
    let backticks (source:Text) : Text = 
        enclose (rawchar '`') (rawchar '`') source

    /// Backticks for inline code.
    let doubleBackticks (source:Text) : Text = 
        enclose (rawtext "``") (rawtext "``") source


    let private useReference (altText:string) (identifier:string) : Text = 
        squareBrackets (text altText) ^^ squareBrackets (text identifier)

    /// [Alt text][id]
    let useLinkReference (altText:string) (identifier:string) : Text = 
        hgroup (useReference altText identifier)

    /// ![Alt text][id]
    let useImageReference (altText:string) (identifier:string) : Text = 
        hgroup (bang ^^ useReference altText identifier)
        
    


    let private inlineLinkBody (altText:string) 
                               (path:string) 
                               (title:option<string>) : Text = 
        let path1 = Common.replaceBackslashes path
        let body : Text = 
            match title with
            | None -> rawtext path1
            | Some ss -> rawtext path1 ^+^ text ss
        squareBrackets (text altText) ^^ parens body

    /// [Alt text](/path/to)
    ///
    /// [Alt text](/path/to "Title") 
    ///
    /// Note - if path uses backslash as a separator (Windows style) 
    /// it is rewritten to use forward slash (Unix style and Pandoc style).
    /// 
    /// The path should not be explicitly quoted even if it contains spaces.
    let inlineLink (altText:string) (path:string) (title:option<string>) : Text = 
        hgroup (inlineLinkBody altText path title) 


    /// ![Alt text](/path/to)
    ///
    /// ![Alt text](/path/to "Title") 
    ///
    /// Note - if path uses backslash as a separator (Windows style) 
    /// it is rewritten to use forward slash (Unix style and Pandoc style).
    /// 
    /// The path should not be explicitly quoted even if it contains spaces.
    let inlineImage (altText:string) 
                    (path:string) 
                    (title:option<string>) : Text = 
        hgroup (bang ^^ inlineLinkBody altText path title) 


    let formatted (fmt:Printf.StringFormat<'a -> string,string>) 
                  (value:'a) : Text = 
        sprintf fmt value |> text


    /// Print a unsigned byte literal as a decimal.
    /// Note no F# type specifying suffix is printed, if you want this
    /// functionality you need to write your own function.
    let byteDoc (i:byte) : Text = 
        i.ToString() |> text
        
    /// Print a signed byte literal as a decimal.
    let sbyteDoc (i:sbyte) : Text = 
        i.ToString() |> text

    /// Print a 16-bit signed byte literal as a decimal.
    let int16Doc (i:int16) : Text = 
        i.ToString() |> text

    /// Print a 16-bit unsigned byte literal as a decimal.
    let uint16Doc (i:uint16) : Text = 
        i.ToString() |> text

    /// Print a 32-bit signed byte literal as a decimal.
    let int32Doc (i:int32) : Text = 
        i.ToString() |> text

    /// Print a 32-bit unsigned byte literal as a decimal.
    let uint32Doc (i:uint32) : Text = 
        i.ToString() |> text

    /// Print a 64-bit signed byte literal as a decimal.        
    let int64Doc (i:int64) : Text = 
        i.ToString() |> text

    /// Print a 64-bit unsigned byte literal as a decimal.
    let uint64Doc (i:uint64) : Text = 
        i.ToString() |> text
    
    /// Print a 32-bit IEEE float. 
    /// The output uses FSharp's ToString() so it may be printed in 
    /// exponential notation.
    let float32Doc (d:float32) : Text = 
        d.ToString() |> text


    // ************************************************************************
    // Paragraph elements


    /// Paragraph assembles Text
    type ParaElement = Syntax.MdParaElement

    let emptyParaElement : ParaElement = Syntax.MdParaElement.empty

    let paraText (text:Text) : ParaElement = 
        Syntax.paragraphText text
    
    /// Vertical concat.
    let ( ^&^ ) (d1:ParaElement) (d2:ParaElement) : ParaElement = 
        Syntax.belowParaElement d1 d2


    let unorderedList (elements:ParaElement list) : ParaElement = 
        Syntax.uList elements

    let orderedList (elements:ParaElement list) : ParaElement = 
        Syntax.oList elements

    let private defReference (identifier:string) 
                             (path:string) 
                             (title:string option) : Text = 
        let body1 = 
            squareBrackets (text identifier) ^^ colon ^+^ text (Common.replaceBackslashes path)
        match title with
        | None -> body1
        | Some ss -> body1 ^+^ doubleQuotes (text ss)

    /// [id]: path/to 
    /// [id]: path/to "Title"
    let defLinkReference (identifier:string) 
                         (path:string) 
                         (title:string option) : ParaElement = 
        paraText (hgroup (defReference identifier path title))


    /// ![id]: path/to 
    /// ![id]: path/to "Title"
    let defImageReference (identifier:string) 
                          (path:string) 
                          (title:string option) : ParaElement = 
         paraText (hgroup (bang ^^ defReference identifier path title))


    /// Tiled markdown i.e. large sections paragraphs, list elements, table cell text...

    /// Probably just formatting width, but opaque anyway...
    type RenderContext = 
        private { ColumnWidth: int }


    /// Markdown is the type for document fragments - paragraph or larger.
    type Markdown = 
        | Markdown of (RenderContext -> Syntax.MdDoc)

        static member empty : Markdown = Markdown (fun _ -> Syntax.MdDoc.empty)

        member internal x.GetMarkdown 
            with get() = match x with | Markdown(fn) -> fn

        member x.SaveToString(columnWidth:int) : string = 
            let doc = x.GetMarkdown {ColumnWidth = columnWidth}
            Syntax.renderMdDoc doc

        member x.SaveToString() : string = 
            x.SaveToString(columnWidth = 80)

        member x.Save(sw:StreamWriter) : unit = 
            let str = x.SaveToString(columnWidth = 80)
            sw.Write(str)

        member x.Save(columnWidth:int, sw:StreamWriter) : unit = 
            let str = x.SaveToString(columnWidth = columnWidth)
            sw.Write(str)

        member x.Save (columnWidth:int, outputPath:string) : unit = 
            use sw = new System.IO.StreamWriter(outputPath)
            x.Save(columnWidth, sw)

        member x.Save (outputPath:string) : unit = 
            use sw = new System.IO.StreamWriter(outputPath)
            x.Save(sw)
        


    let localColumnWidth (columnWidth:int) (doc:Markdown) : Markdown = 
        Markdown <| fun ctx -> 
            doc.GetMarkdown { ctx with ColumnWidth = columnWidth }
    
    
    let emptyMarkdown : Markdown = Markdown.empty

    /// Lift paragraph contents to Markdown.
    let markdown (paragraph:ParaElement) : Markdown = 
        Markdown <| fun ctx -> 
            Syntax.markdownParagraph ctx.ColumnWidth paragraph

    /// Lift paragraph contents to Markdown, render to 
    /// the spcified width.
    let fixedWidthMarkdown (width:int) (paragraph:ParaElement) : Markdown = 
        Markdown <| fun _ -> 
            Syntax.markdownParagraph width paragraph

    /// Formatted according to columnWidth
    let markdownText (text:Text) : Markdown = 
        markdown (paraText text)

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
    let codeBlock (body:ParaElement) : Markdown = 
        Markdown <| fun ctx -> Syntax.codeParagraph body

    /// Concatenate two Markdown fragments.
    let ( ^@^ ) (a:Markdown) (b:Markdown) : Markdown = 
        Markdown <| fun ctx -> 
            let (Markdown f1) = a 
            let (Markdown f2) = b
            Syntax.belowDoc (f1 ctx) (f2 ctx)

    /// Concatenate a list of Markdown fragments.        
    let concatMarkdown (elements:Markdown list) : Markdown = 
        Markdown <| fun ctx ->
            let tiles = List.map (fun (doc:Markdown) -> doc.GetMarkdown ctx) elements
            Syntax.belowDocs tiles

    /// nbsp
    let nbsp : Markdown = markdownText (entity "nbsp")


    /// Page break / Horizontal Rule
    /// Printed as five asterisks.
    let horizontalRule : Markdown = markdownText (text "*****")




    /// Print the Text to the console.
    let testRenderText (source:Text) : unit = 
        Syntax.renderMdText 80 source |> printfn  "----------\n%s\n----------\n"


    let testRender (source:Markdown) : unit = 
        source.SaveToString() |> printfn  "----------\n%s\n----------\n"