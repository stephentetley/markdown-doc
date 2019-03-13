// Copyright (c) Stephen Tetley 2018,2019
// License: BSD 3 Clause

namespace MarkdownDoc

// Design issue
// Whitespace matters in Pandoc.
// With the different doc fragments type we should be handling
// it implicitly - check this assumption!


[<AutoOpen>]
module Markdown = 

    open System.IO

    open MarkdownDoc.Internal
    open Internal.Syntax
    
    type Alignment = Syntax.Alignment
    type ColumnSpec = Syntax.ColumnSpec


    /// Text is the type for 'body text'. 
    /// Sentences and markup smaller than a paragraph.
    type Text = Syntax.MdText

    /// The empty Text document.
    let emptyText : Text = Text.empty

    /// Bind the text into a unbreakable group
    let hgroup (text:Text) : Text = 
        Syntax.Group text

    /// Build a Text item from a single char. 
    /// '&' and '<' will be escaped.
    let character (ch:char) : Text = 
        match ch with
        | '<' -> Syntax.Text "&lt;"
        | '&' -> Syntax.Text "&amp;"
        | _ -> Syntax.Text <| ch.ToString()

    /// Build a Text item from a string. 
    /// '&' and '<' will be escaped.
    let text (content:string) : Text = 
        /// Ampersand must be replaced first, otherwise we get double escaping.
        let s1 = content.Replace("&", "&amp;").Replace("<", "&lt;")
        Syntax.Text s1  
        
    /// Build a Text item from a string. 
    /// No escaping is performed, use this function with care.
    let plaintext (content:string) : Text = 
        Syntax.Text content  

    let plainlines (contents:string list) : Text = 
        Syntax.textlines <| List.map plaintext contents

    let rawText (source:string) : Text = 
        Syntax.RawText source

    /// Print the Text to the console.
    let testRenderText (source:Text) : unit = 
        Syntax.renderMdText 80 source |> printfn  "----------\n%s\n----------\n"

    /// Horizontal concat directly (no separating space)
    let ( ^^ ) (d1:Text) (d2:Text) : Text = 
        Syntax.beside d1 d2

    /// Horizontal concat with a separating space 
    let ( ^+^ ) (d1:Text) (d2:Text) : Text = 
        Syntax.besideSpace d1 d2

    /// Vertical concat.
    let ( ^&^ ) (d1:Text) (d2:Text) : Text = 
        Syntax.belowText d1 d2

    let textlines (lines:Text list) : Text = 
        Syntax.textlines lines


    let bang : Text = character '!'
    let colon : Text = character ':'
    let space : Text = character ' '
    let equals : Text = character '='

    let entity (name:string) : Text = plaintext <| sprintf "&%s;" name



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
    let backticks3 : Text = plaintext "```"


    let enclose (left:Text) (right:Text) (d1:Text) : Text = 
        left ^^ d1 ^^ right


    let parens (source:Text) : Text = 
        enclose (character '(') (character ')') source

    let squareBrackets (source:Text) : Text = 
        enclose (character '[') (character ']') source

    /// Can be used for inlining links.
    let angleBrackets (source:Text) : Text = 
        enclose (character '<') (character '>') source

    /// Curly braces
    let braces (source:Text) : Text = 
        enclose (character '{') (character '}') source


    let singleQuotes (source:Text) : Text = 
        enclose (character '\'') (character '\'') source

    let doubleQuotes (source:Text) : Text = 
        enclose (character '"') (character '"') source

    /// Emphasis
    let asterisks (source:Text) : Text = 
        enclose (character '*') (character '*') source

    /// Emphasis
    let underscores (source:Text) : Text = 
        enclose (character '_') (character '_') source

    /// Strong emphasis
    let doubleAsterisks (source:Text) : Text = 
        enclose (text "**") (text "**") source

    /// Strong emphasis
    let doubleUnderscores (source:Text) : Text = 
        enclose (text "__") (text "__") source

    /// Backticks for inline code.
    let backticks (source:Text) : Text = 
        enclose (character '`') (character '`') source

    /// Backticks for inline code.
    let doubleBackticks (source:Text) : Text = 
        enclose (text "``") (text "``") source


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
            | None -> rawText path1
            | Some ss -> rawText path1 ^+^ text ss
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
    type ParaElement = Syntax.MdPElement

    let emptyParaElement : ParaElement = MdPElement.empty

    let paraText (text:Text) : ParaElement = 
        Syntax.ParaText text
    
    /// Vertical concat.
    let ( ^/^ ) (d1:ParaElement) (d2:ParaElement) : ParaElement = 
        Syntax.belowPElement d1 d2


    let unordList (elements:ParaElement list) : ParaElement = 
        Syntax.UnorderedList elements

    let ordList (elements:ParaElement list) : ParaElement = 
        Syntax.OrderedList elements

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

        static member empty : Markdown = Markdown (fun _ -> MdDoc.empty)

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


    let markdown (paragraph:ParaElement) : Markdown = 
        Markdown <| fun ctx -> 
            Syntax.Paragraph(ctx.ColumnWidth, paragraph)


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
        Markdown <| fun ctx -> Syntax.CodeBlock(body)

    /// Concatenate two Markdown fragments.
    let ( ^@^ ) (a:Markdown) (b:Markdown) : Markdown = 
        Markdown <| fun ctx -> 
            let (Markdown f1) = a 
            let (Markdown f2) = b
            Syntax.VCatDoc(f1 ctx, f2 ctx)

    /// Concatenate a list of Markdown fragments.        
    let concatMarkdown (elements:Markdown list) : Markdown = 
        Markdown <| fun ctx ->
            let tiles = List.map (fun (doc:Markdown) -> doc.GetMarkdown ctx) elements
            Syntax.concatMdDocs tiles

    /// nbsp
    let nbsp : Markdown = markdownText (entity "nbsp")


    /// Page break / Horizontal Rule
    /// Printed as five asterisks.
    let horizontalRule : Markdown = markdownText (text "*****")


    let testRender (source:Markdown) : unit = 
        source.SaveToString() |> printfn  "----------\n%s\n----------\n"