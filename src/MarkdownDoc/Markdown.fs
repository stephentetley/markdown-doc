// Copyright (c) Stephen Tetley 2018
// License: BSD 3 Clause

namespace MarkdownDoc

/// Design issue
/// Whitespace matters in Pandoc.
/// How explicitly should we treat it in this library?
/// Do we help the user (making the implementation complicated) or keep 
/// the implementation simple relying on the user to do the right thin?

open System.IO

open MarkdownDoc.Internal


[<AutoOpen>]
module Markdown = 

    type Text = SimpleText.Text

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

    /// Horizontal concat with a separating space 
    let (^+^) (d1:Text) (d2:Text) : Text = 
       d1 ^^ character ' ' ^^ d2

    let bang : Text = character '!'
    let colon : Text = character ':'
    let space : Text = character ' '

    let nbsp : Text = text "&nbsp;"

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

    /// Probably just line width, but opaque anyway...
    type RenderContext = 
        private { LineWidth: int }


    
    type Markdown = 
        | Markdown of (RenderContext -> Tile.Tile)

        member x.Render(lineWidth:int) : string = 
            let fn = match x with | Markdown(fx) -> fx 
            let tile = fn {LineWidth = lineWidth}
            Tile.render tile

        member x.Render() : string = x.Render(lineWidth = 80)

        member x.Save(sw:StreamWriter) : unit = 
            sw.Write(x.Render())

    let inline private getMarkdown (doc:Markdown) : RenderContext -> Tile.Tile = 
        let (Markdown fn) = doc in fn 


    let render (lineWidth:int) (doc:Markdown) : string = 
        let fn = getMarkdown doc 
        let tile = fn {LineWidth = lineWidth}
        Tile.render tile

    let renderFile (lineWidth:int) (outputPath:string) (doc:Markdown) : unit = 
        let md = doc.Render(lineWidth=lineWidth)
        System.IO.File.WriteAllText(path = outputPath, contents = md)
        

    let testRender (source:Markdown) : unit = 
        source.Render(lineWidth = 80) |> printfn  "----------\n%s\n----------\n"

    let localLineWidth (lineWidth:int) (doc:Markdown) : Markdown = 
        Markdown <| fun ctx -> 
            let fn = getMarkdown doc 
            fn { ctx with LineWidth = lineWidth }

    let tile (text:Text) : Markdown = 
        Markdown <| fun ctx -> 
            Tile.tile ctx.LineWidth text




    let preformatted (lines:Text list) : Markdown = 
        Markdown <| fun _ -> 
            Tile.preformatted lines


    let private tileMap (fn:Tile.Tile -> Tile.Tile) (doc:Markdown) : Markdown = 
        let mf = getMarkdown doc
        Markdown <| fun ctx -> fn (mf ctx)


    let h1 (content:Text) : Markdown = tile (text "#" ^+^ content)
    let h2 (content:Text) : Markdown = tile (text "##" ^+^ content)
    let h3 (content:Text) : Markdown = tile (text "###" ^+^ content)
    let h4 (content:Text) : Markdown = tile (text "####" ^+^ content)
    let h5 (content:Text) : Markdown = tile (text "#####" ^+^ content)
    let h6 (content:Text) : Markdown = tile (text "######" ^+^ content)

    


    let codeBlock (tile:Markdown) : Markdown = 
        tileMap (Tile.prefixAll "    ") tile

    /// Concatenate two Markdown pieces.
    let (^@^) (a:Markdown) (b:Markdown) : Markdown = 
        Markdown <| fun ctx -> 
            let (Markdown f1) = a 
            let (Markdown f2) = b
            f1 ctx + f2 ctx

    let concat (elements:Markdown list) : Markdown = 
        Markdown <| fun ctx ->
            let tiles = List.map (fun (e:Markdown) -> let mf = getMarkdown e in mf ctx) elements
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
        localLineWidth 300 (tile <| text)


    let defImageReference (identifier:string) (path:string) (title:option<string>) : Markdown = 
        let title1  = 
            match title with
            | None -> empty
            | Some str -> space ^^ doubleQuotes (text str)
        let text = squareBrackets (text identifier) ^^ colon ^+^ text path ^^ title1
        localLineWidth 300 (tile <| text)


    type Alignment = MarkdownDoc.Internal.Common.Alignment
    type ColumnSpec = MarkdownDoc.Internal.Common.ColumnSpec

    let gridTable (columnSpecs:ColumnSpec list) (contents: (Markdown list) list) 
                        (hasHeaders:bool) : Markdown = 
        Markdown <| fun ctx ->
            let renderCell (spec:ColumnSpec) (doc:Markdown) : Tile.CellText = 
                let mf = getMarkdown doc
                let tile = mf { ctx with LineWidth = spec.Width }
                Tile.getLines tile
            let renderRow (row: Markdown list) : Tile.CellText list = 
                List.map2 renderCell columnSpecs row
            let contents1 = List.map renderRow contents 
            Tile.textGridTable columnSpecs contents1 hasHeaders
