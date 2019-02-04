// Copyright (c) Stephen Tetley 2018,2019
// License: BSD 3 Clause

namespace MarkdownDoc

// Design issue
// Whitespace matters in Pandoc.
// How explicitly should we treat it in this library?
// Do we help the user (making the implementation complicated) or keep 
// the implementation simple relying on the user to do the right thing?


[<AutoOpen>]
module Markdown = 

    open System.IO

    open MarkdownDoc.Internal
    
    /// Text is the type for 'body text'. 
    /// Sentences and markup smaller than a paragraph.
    type Text = Syntax.MdText

    /// The empty Text document.
    let empty : Text = Syntax.NoText

    /// Build a Text item from a single char. 
    /// '&' and '<' will be escaped.
    let character (ch:char) : Text = 
        match ch with
        | '<' -> Syntax.String "&lt;"
        | '&' -> Syntax.String "&amp;"
        | _ -> Syntax.String <| ch.ToString()

    /// Build a Text item from a string. 
    /// '&' and '<' will be escaped.
    let text (content:string) : Text = 
        /// Ampersand must be replaced first, otherwise we get double escaping.
        let s1 = content.Replace("&", "&amp;").Replace("<", "&lt;")
        Syntax.String s1  
        
    /// Build a Text item from a string. 
    /// No escaping is performed, use this function with care.
    let rawtext (content:string) : Text = 
        Syntax.String content  

    let rawlines (contents:string list) : Text = 
        Syntax.textlines <| List.map rawtext contents

    /// Print the Text to the console.
    let testRenderText (source:Text) : unit = 
        Syntax.renderMdText source |> printfn  "----------\n%s\n----------\n"

    /// Horizontal concat directly (no separating space)
    let ( ^^ ) (d1:Text) (d2:Text) : Text = 
        Syntax.beside d1 d2

    /// Horizontal concat with a separating space 
    let ( ^+^ ) (d1:Text) (d2:Text) : Text = 
        Syntax.besideSpace d1 d2

    /// Vertical concat.
    let ( ^&^ ) (d1:Text) (d2:Text) : Text = 
        Syntax.below d1 d2

    let textlines (lines:Text list) : Text = 
        Syntax.textlines lines


    let bang : Text = character '!'
    let colon : Text = character ':'
    let space : Text = character ' '
    let equals : Text = character '='

    let entity (name:string) : Text = rawtext <| sprintf "&%s;" name

    let nbsp : Text = entity "nbsp"

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

    

    /// [A link](/path/to)
    ///
    /// [A link](/path/to "Title") 
    let inlineLink (altText:Text) (path:string) (title:option<string>) : Text = 
        let title1  = 
            match title with
            | None -> empty
            | Some ss -> space ^^ doubleQuotes (text ss)
        (squareBrackets altText) ^^ parens (text path ^^ title1)


    let inlineImage (altText:Text) (path:string) (title:option<string>) : Text = 
        let title1  = 
            match title with
            | None -> empty
            | Some ss -> space ^^ doubleQuotes (text ss)
        bang ^^ (squareBrackets altText) ^^ parens (text path ^^ title1)


    let useLinkReference (altText:Text) (identifier:string) : Text = 
        squareBrackets altText ^^ squareBrackets (text identifier)

    let useImageReference (altText:Text) (identifier:string) : Text = 
        bang ^^ (squareBrackets altText) ^^ (squareBrackets <| text identifier)

    /// Paragraph assembles Text
    type Paragraph = Syntax.MdPara

    let paraTile (text:Text) : Paragraph = 
        Syntax.ParaText text

    let unordList (elements:Paragraph list) : Paragraph = 
        Syntax.UnorderedList elements

    let ordList (elements:Paragraph list) : Paragraph = 
        Syntax.OrderedList elements

    /// Tiled markdown i.e. large sections paragraphs, list elements, table cell text...

    /// Probably just formatting width, but opaque anyway...
    type RenderContext = 
        private { ColumnWidth: int }


    /// Markdown is the type for document fragments - paragraph or larger.
    type Markdown = 
        | Markdown of (RenderContext -> Syntax.MdDoc)

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
        

    let testRender (source:Markdown) : unit = 
        source.SaveToString() |> printfn  "----------\n%s\n----------\n"

    let localColumnWidth (columnWidth:int) (doc:Markdown) : Markdown = 
        Markdown <| fun ctx -> 
            doc.GetMarkdown { ctx with ColumnWidth = columnWidth }
    
    let markdown (para:Paragraph) : Markdown = 
        Markdown <| fun ctx -> 
            Syntax.Paragraph para

    /// TODO line breaking?
    let markdownTile (text:Text) : Markdown = 
        Markdown <| fun ctx -> 
            Syntax.Paragraph (Syntax.ParaText text)

    ///// Does not line break.
    //let preformatted (text:Text) : Markdown = 
    //    Markdown <| fun _ -> 
    //        MarkdownTile.preformatted text

    ///// Does not line break.
    //let preformattedLines (lines:Text list) : Markdown = 
    //    Markdown <| fun _ -> 
    //        MarkdownTile.preformattedLines lines


    //let private tileMap (fn:Syntax.MdDoc -> Syntax.MdDoc) (doc:Markdown) : Markdown = 
    //    let mf = doc.GetMarkdown
    //    Markdown <| fun ctx -> fn (mf ctx)

    /// Atx style header H1
    let h1 (content:Text) : Markdown = markdownTile (text "#" ^+^ content)
    
    /// Atx style header H2
    let h2 (content:Text) : Markdown = markdownTile (text "##" ^+^ content)

    /// Atx style header H3
    let h3 (content:Text) : Markdown = markdownTile (text "###" ^+^ content)

    /// Atx style header H4
    let h4 (content:Text) : Markdown = markdownTile (text "####" ^+^ content)

    /// Atx style header H5
    let h5 (content:Text) : Markdown = markdownTile (text "#####" ^+^ content)

    /// Atx style header H6
    let h6 (content:Text) : Markdown = markdownTile (text "######" ^+^ content)

    

    /// Code block indents the paragraph with four spaces.
    let codeBlock (para:Paragraph) : Markdown = 
        Markdown <| fun ctx -> Syntax.CodeBlock(para)

    /// Concatenate two Markdown fragments.
    let ( ^@^ ) (a:Markdown) (b:Markdown) : Markdown = 
        Markdown <| fun ctx -> 
            let (Markdown f1) = a 
            let (Markdown f2) = b
            Syntax.VCatDoc(f1 ctx, f2 ctx)

    let concatMarkdown (elements:Markdown list) : Markdown = 
        Markdown <| fun ctx ->
            let tiles = List.map (fun (doc:Markdown) -> doc.GetMarkdown ctx) elements
            Syntax.concatMdDocs tiles

    //let tiles (paragraphs:Text list) : Markdown = 
    //    concat <| List.map tile paragraphs 

        

    //let defLinkReference (identifier:string) (path:string) (title:option<string>) : Markdown = 
    //    let title1  = 
    //        match title with
    //        | None -> empty
    //        | Some ss -> space ^^ doubleQuotes (text ss)
    //    let text = squareBrackets (text identifier) ^^ colon ^+^ angleBrackets (text path) ^^ title1
    //    // Potentially we need a non-breaking version of tile.
    //    localColumnWidth 300 (tile <| text)


    //let defImageReference (identifier:string) (path:string) (title:option<string>) : Markdown = 
    //    let title1  = 
    //        match title with
    //        | None -> empty
    //        | Some str -> space ^^ doubleQuotes (text str)
    //    let text = squareBrackets (text identifier) ^^ colon ^+^ text path ^^ title1
    //    localColumnWidth 300 (tile <| text)


    //type Alignment = Common.Alignment
    //type ColumnSpec = Common.ColumnSpec

    //let gridTable (columnSpecs:ColumnSpec list) 
    //              (contents: (Markdown list) list) 
    //              (hasHeaders:bool) : Markdown = 
    //    Markdown <| fun ctx ->
    //        let renderCell (spec:ColumnSpec) (doc:Markdown) : MarkdownTile.CellText = 
    //            let tile = doc.GetMarkdown { ctx with ColumnWidth = spec.Width }
    //            tile.TextLines
    //        let renderRow (row: Markdown list) : MarkdownTile.CellText list = 
    //            List.map2 renderCell columnSpecs row
    //        let contents1 = List.map renderRow contents 
    //        MarkdownTile.textGridTable columnSpecs contents1 hasHeaders
