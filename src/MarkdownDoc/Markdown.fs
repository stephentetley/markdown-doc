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
    
    type Alignment = Syntax.Alignment
    type ColumnSpec = Syntax.ColumnSpec


    /// Text is the type for 'body text'. 
    /// Sentences and markup smaller than a paragraph.
    type Text = Syntax.MdText

    /// The empty Text document.
    let empty : Text = Syntax.EmptyText

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
        Syntax.below d1 d2

    let textlines (lines:Text list) : Text = 
        Syntax.textlines lines


    let bang : Text = character '!'
    let colon : Text = character ':'
    let space : Text = character ' '
    let equals : Text = character '='

    let entity (name:string) : Text = plaintext <| sprintf "&%s;" name

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




    let useLinkReference (altText:Text) (identifier:string) : Text = 
        squareBrackets altText ^^ squareBrackets (text identifier)

    let useImageReference (altText:Text) (identifier:string) : Text = 
        bang ^^ (squareBrackets altText) ^^ (squareBrackets <| text identifier)

    /// Paragraph assembles Text
    type Paragraph = Syntax.MdPara

    let paraTile (text:Text) : Paragraph = 
        Syntax.ParaText text

    
    /// [A link](/path/to)
    ///
    /// [A link](/path/to "Title") 
    /// Note - if path uses backslash as a separator (Windows style) 
    /// it is rewritten to use forward slash (Unix style and Pandoc style).
    let inlineLink (altText:string) (path:string) (title:option<string>) : Paragraph = 
        let path1 = path.Replace('\\', '/')
        let body = 
            match title with
            | None -> rawText path1
            | Some ss -> rawText path1 ^^ rawText ss
        squareBrackets (rawText altText) ^+^ parens body |> paraTile



    let inlineImage (altText:string) (path:string) (title:option<string>) : Paragraph = 
        inlineLink altText path title

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
            Syntax.Paragraph(ctx.ColumnWidth, para)


    /// Formatted according to columnWidth
    let markdownTile (text:Text) : Markdown = 
        Markdown <| fun ctx -> 
            Syntax.Paragraph(ctx.ColumnWidth, Syntax.ParaText text)



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

        

    let defLinkReference (identifier:string) (path:string) (title:option<string>) : Markdown = 
        let title1  = 
            match title with
            | None -> empty
            | Some ss -> space ^^ doubleQuotes (text ss)
        let text = squareBrackets (text identifier) ^^ colon ^+^ angleBrackets (text path) ^^ title1
        markdownTile text


    let defImageReference (identifier:string) (path:string) (title:option<string>) : Markdown = 
        let title1  = 
            match title with
            | None -> empty
            | Some str -> space ^^ doubleQuotes (text str)
        let text = squareBrackets (text identifier) ^^ colon ^+^ text path ^^ title1
        markdownTile text


    let gridTable (columnSpecs:ColumnSpec list) 
                  (headers: (Paragraph list) option)
                  (contents: (Paragraph list) list) : Markdown = 

        let makeCell (spec:ColumnSpec) (para:Paragraph) : Syntax.TableCell = 
            { Width = spec.Width
              Content = para }

        let makeRow (row:Paragraph list) : Syntax.TableRow = 
            Common.raggedMap2 makeCell columnSpecs row

        Markdown <| fun _ ->
            let rows = List.map makeRow contents
            Syntax.Table(columnSpecs, Option.map makeRow headers, rows)