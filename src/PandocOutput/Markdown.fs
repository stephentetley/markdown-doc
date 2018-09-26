// Copyright (c) Stephen Tetley 2018
// License: BSD 3 Clause

namespace PandocOutput

/// Design issue
/// Whitespace matters in Pandoc.
/// How explicitly should we treat it in this library?
/// Do we help the user (making the implementation complicated) or keep 
/// the implementation simple relying on the user to do the right thin?



open PandocOutput.Internal


[<AutoOpen>]
module Markdown = 

    type Text = SimpleText.SimpleText

    let empty : Text = SimpleText.Empty

    let character (ch:char) : Text = SimpleText.Raw <| ch.ToString()

    /// TODO - should probably also have a version that does escaping...
    let rawtext (text:string) : Text = SimpleText.Raw text    

    /// Print the Text to the console.
    let testRenderText (source:Text) : unit = 
        SimpleText.renderText1 source |> printfn  "----------\n%s\n----------\n"

    /// Horizontal concat with a separating space 
    let (<+>) (d1:Text) (d2:Text) : Text = 
       d1 + character ' ' + d2

    let bang : Text = character '!'
    let colon : Text = character ':'
    let space : Text = character ' '

    let enclose (left:Text) (right:Text) (d1:Text) : Text = 
        left + d1 + right


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
        enclose (rawtext "**") (rawtext "**") source

    /// Strong emphasis
    let doubleUnderscores (source:Text) : Text = 
        enclose (rawtext "__") (rawtext "__") source

    /// Backticks for inline code.
    let backticks (source:Text) : Text = 
        enclose (character '`') (character '`') source

    /// Backticks for inline code.
    let doubleBackticks (source:Text) : Text = 
        enclose (rawtext "``") (rawtext "``") source

    /// [A link](/path/to)
    ///
    /// [A link](/path/to "Title") 
    let inlineLink (altText:Text) (path:string) (title:option<string>) : Text = 
        let title1  = 
            match title with
            | None -> empty
            | Some ss -> space + doubleQuotes (rawtext ss)
        (squareBrackets altText) + parens (rawtext path + title1)


    let inlineImage (altText:Text) (path:string) (title:option<string>) : Text = 
        let title1  = 
            match title with
            | None -> empty
            | Some ss -> space + doubleQuotes (rawtext ss)
        bang + (squareBrackets altText) + parens (rawtext path + title1)

    /// Probably just line width, but opaque anyway...
    type RenderContext = 
        private { LineWidth: int }


    
    type Markdown = 
        | Markdown of (RenderContext -> Tile.Tile)

    let render (lineWidth:int) (doc:Markdown) : string = 
        let (Markdown fn) = doc 
        let tile = fn {LineWidth = lineWidth}
        Tile.render tile
    
    let testRender (source:Markdown) : unit = 
        render 80 source |> printfn  "----------\n%s\n----------\n"


    let tile (text:Text) : Markdown = 
        Markdown <| fun ctx -> 
            Tile.tile ctx.LineWidth text

