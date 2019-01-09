// Copyright (c) Stephen Tetley 2018,2019
// License: BSD 3 Clause

namespace MarkdownDoc

/// Design issue
/// Whitespace matters in Pandoc.
/// How explicitly should we treat it in this library?
/// Do we help the user (making the implementation complicated) or keep 
/// the implementation simple relying on the user to do the right thing?




[<AutoOpen>]
module Markdown = 

    open System.IO

    open MarkdownDoc.Internal
    
    /// Text is the type for 'body text'. 
    /// Sentences and markup smaller than a paragraph.
    type Text = SimpleText.Text

    /// The empty Text document.
    let empty : Text = SimpleText.Empty

    /// Build a Text item from a single char. 
    /// '&' and '<' will be escaped.
    let character (ch:char) : Text = 
        match ch with
        | '<' -> SimpleText.String "&lt;"
        | '&' -> SimpleText.String "&amp;"
        | _ -> SimpleText.String <| ch.ToString()

    /// Build a Text item from a string. 
    /// '&' and '<' will be escaped.
    let text (content:string) : Text = 
        /// Ampersand must be replaced first, otherwise we get double escaping.
        let s1 = content.Replace("&", "&amp;").Replace("<", "&lt;")
        SimpleText.String s1  
        
    /// Build a Text item from a string. 
    /// No escaping is performed, use this function with care.
    let rawtext (content:string) : Text = 
        SimpleText.String content  

    /// Print the Text to the console.
    let testRenderText (source:Text) : unit = 
        SimpleText.renderText1 source |> printfn  "----------\n%s\n----------\n"

    /// Horizontal concat directly (no separating space)
    let (^^) (d1:Text) (d2:Text) : Text = 
       SimpleText.beside d1 d2

    /// Horizontal concat with a separating space 
    let (^+^) (d1:Text) (d2:Text) : Text = 
       SimpleText.besideSpace d1 d2

    let textlines (lines:Text list) : Text = 
        SimpleText.textlines lines


    let bang : Text = character '!'
    let colon : Text = character ':'
    let space : Text = character ' '

    let nbsp : Text = rawtext "&nbsp;"

    let enclose (left:Text) (right:Text) (d1:Text) : Text = 
        left ^^ d1 ^^ right


    let parens (source:Text) : Text = 
        enclose (character '(') (character ')') source

    let squareBrackets (source:Text) : Text = 
        enclose (character '[') (character ']') source

    /// Can be used for inlining links.
    let angleBrackets (source:Text) : Text = 
        enclose (character '<') (character '>') source

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




    /// Tiled markdown i.e. large sections paragraphs, list elements, table cell text...

    /// Probably just formatting width, but opaque anyway...
    type RenderContext = 
        private { ColumnWidth: int }


    /// Markdown is the type for document fragments - paragraph or larger.
    type Markdown = 
        | Markdown of (RenderContext -> Tile.Tile)

        member internal x.GetMarkdown 
            with get() = match x with | Markdown(fn) -> fn

        member x.SaveToString(columnWidth:int) : string = 
            let tile = x.GetMarkdown {ColumnWidth = columnWidth}
            tile.SaveToString()

        member x.SaveToString() : string = 
            x.SaveToString(columnWidth = 80)

        member x.Save(sw:StreamWriter) : unit = 
            let tile = x.GetMarkdown {ColumnWidth = 80}
            tile.Save(sw)

        member x.Save(columnWidth:int, sw:StreamWriter) : unit = 
            let tile = x.GetMarkdown {ColumnWidth = columnWidth}
            tile.Save(sw)

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

    let tile (text:Text) : Markdown = 
        Markdown <| fun ctx -> 
            Tile.tile ctx.ColumnWidth text




    let preformatted (lines:Text list) : Markdown = 
        Markdown <| fun _ -> 
            Tile.preformatted lines


    let private tileMap (fn:Tile.Tile -> Tile.Tile) (doc:Markdown) : Markdown = 
        let mf = doc.GetMarkdown
        Markdown <| fun ctx -> fn (mf ctx)


    let h1 (content:Text) : Markdown = tile (text "#" ^+^ content)
    let h2 (content:Text) : Markdown = tile (text "##" ^+^ content)
    let h3 (content:Text) : Markdown = tile (text "###" ^+^ content)
    let h4 (content:Text) : Markdown = tile (text "####" ^+^ content)
    let h5 (content:Text) : Markdown = tile (text "#####" ^+^ content)
    let h6 (content:Text) : Markdown = tile (text "######" ^+^ content)

    


    let codeBlock (tile:Markdown) : Markdown = 
        tileMap (Tile.prefixAll "    ") tile

    /// Concatenate two Markdown fragments.
    let (^@^) (a:Markdown) (b:Markdown) : Markdown = 
        Markdown <| fun ctx -> 
            let (Markdown f1) = a 
            let (Markdown f2) = b
            f1 ctx + f2 ctx

    let concat (elements:Markdown list) : Markdown = 
        Markdown <| fun ctx ->
            let tiles = List.map (fun (doc:Markdown) -> doc.GetMarkdown ctx) elements
            Tile.concat tiles

    let tiles (paragraphs:Text list) : Markdown = 
        concat <| List.map tile paragraphs 


    let unordList (elements:Markdown list) : Markdown = 
        concat <| List.map (tileMap (Tile.prefixFirstRest "* " "  ")) elements

    let ordList (elements:Markdown list) : Markdown = 
        let listItem (ix:int) (doc:Markdown) = 
            tileMap (Tile.prefixFirstRest (sprintf "%i. " (ix+1)) "  ") doc
        concat <| List.mapi listItem elements 
        

    let defLinkReference (identifier:string) (path:string) (title:option<string>) : Markdown = 
        let title1  = 
            match title with
            | None -> empty
            | Some ss -> space ^^ doubleQuotes (text ss)
        let text = squareBrackets (text identifier) ^^ colon ^+^ angleBrackets (text path) ^^ title1
        // Potentially we need a non-breaking version of tile.
        localColumnWidth 300 (tile <| text)


    let defImageReference (identifier:string) (path:string) (title:option<string>) : Markdown = 
        let title1  = 
            match title with
            | None -> empty
            | Some str -> space ^^ doubleQuotes (text str)
        let text = squareBrackets (text identifier) ^^ colon ^+^ text path ^^ title1
        localColumnWidth 300 (tile <| text)


    type Alignment = Common.Alignment
    type ColumnSpec = Common.ColumnSpec

    let gridTable (columnSpecs:ColumnSpec list) (contents: (Markdown list) list) 
                        (hasHeaders:bool) : Markdown = 
        Markdown <| fun ctx ->
            let renderCell (spec:ColumnSpec) (doc:Markdown) : Tile.CellText = 
                let tile = doc.GetMarkdown { ctx with ColumnWidth = spec.Width }
                tile.TextLines
            let renderRow (row: Markdown list) : Tile.CellText list = 
                List.map2 renderCell columnSpecs row
            let contents1 = List.map renderRow contents 
            Tile.textGridTable columnSpecs contents1 hasHeaders
