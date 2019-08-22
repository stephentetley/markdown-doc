// Copyright (c) Stephen Tetley 2018,2019
// License: BSD 3 Clause

namespace MarkdownDoc.Markdown



[<AutoOpen>]
module Text = 

    open System.IO

    open MarkdownDoc.Internal
    

    /// Text is the type for 'body text'. 
    /// Sentences and markup smaller than a paragraph.
    type Text = Doc.MdText

    /// The empty Text document.
    let emptyText : Text = Doc.EmptyText

    /// Bind the text into a unbreakable group
    let hgroup (text : Text) : Text = 
        Doc.Group text


    /// Build a Text item from a single char. 
    /// '&' and '<' will be escaped.
    let character (ch : char) : Text = 
        match ch with
        | '<' -> Doc.Text "&lt;"
        | '&' -> Doc.Text "&amp;"
        | _ -> Doc.Text <| ch.ToString()

    /// Build a Text item from a single char. 
    /// No escaping.
    let rawchar (ch : char) : Text = Doc.Text <| ch.ToString()
        

    let private escapedText (content : string) : Text = 
        /// Ampersand must be replaced first, otherwise we get double escaping.
        let s1 = content.Replace("&", "&amp;").Replace("<", "&lt;")
        Doc.Text  s1  

    /// Build a Text item from a string. 
    /// '&' and '<' will be escaped.
    /// No line splitting.
    let text (content : string) : Text = escapedText content  
        
    /// Build a Text item from a string. 
    /// No escaping is performed, use this function with care.
    let rawtext (content : string) : Text = 
        Doc.Text content  



    /// Horizontal concat directly (no separating space)
    let ( ^^ ) (text1 : Text) (text2 : Text) : Text = 
        match text1, text2 with
        | Doc.EmptyText, d2 -> d2
        | d1, Doc.EmptyText -> d1
        | d1,d2 -> Doc.HCatText(d1, d2)

    /// Horizontal concat with a separating space 
    let ( ^+^ ) (text1 : Text) (text2 : Text) : Text = 
        match text1, text2 with
        | Doc.EmptyText, d2 -> d2
        | d1, Doc.EmptyText -> d1
        | d1,d2 -> d1 ^^ character ' ' ^^ d2

    /// Vertical concat.
    let ( ^/^ ) (text1 : Text) (text2 : Text) : Text = 
        match text1, text2 with
        | Doc.EmptyText, d2 -> d2
        | d1, Doc.EmptyText -> d1
        | d1,d2 -> Doc.VCatText(d1,d2)

   
    /// Build a text document from a list of Text lines.
    /// The lines are vertically concatenated.
    let textlines (docs : Text list) : Text = 
        match docs with
        | [] -> emptyText
        | d1 :: rest -> List.fold (fun ac d -> ac ^/^ d) d1 rest

    
    /// Build a multiline Text item from a string. 
    /// No escaping is performed, use this function with care.
    let rawlines (contents : string list) : Text = 
        textlines <| List.map rawtext contents

    /// Horizontal concatenate. No separating space.
    let hcat (items : Text list) : Text = 
        List.fold (^^) emptyText items


    /// Horizontal concatenate with a separating space.
    let hsep (items : Text list) : Text = 
        List.fold (^+^) emptyText items

    let bang : Text = character '!'
    let colon : Text = character ':'
    let space : Text = character ' '
    let equalsSign : Text = character '='

    let entity (name : string) : Text = rawtext <| sprintf "&%s;" name



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


    let enclose (left : Text) (right : Text) (d1 : Text) : Text = 
        left ^^ d1 ^^ right

    /// Add to markdown-doc?
    let textPunctuate (separator : Text) (texts : Text list) : Text = 
        match texts with 
        | [] -> emptyText
        | [d1] -> d1
        | d1 :: rest -> List.fold (fun ac d -> ac ^^ separator ^^ d) d1 rest


    let parens (source : Text) : Text = 
        enclose (rawchar '(') (rawchar ')') source

    let squareBrackets (source : Text) : Text = 
        enclose (rawchar '[') (rawchar ']') source

    /// Can be used for inlining links.
    let angleBrackets (source : Text) : Text = 
        enclose (rawchar '<') (rawchar '>') source

    /// Curly braces
    let braces (source : Text) : Text = 
        enclose (rawchar '{') (rawchar '}') source


    let singleQuotes (source : Text) : Text = 
        enclose (rawchar '\'') (rawchar '\'') source

    let doubleQuotes (source : Text) : Text = 
        enclose (rawchar '"') (rawchar '"') source

    /// Emphasis
    let asterisks (source : Text) : Text = 
        enclose (rawchar '*') (rawchar '*') source

    /// Emphasis
    let underscores (source : Text) : Text = 
        enclose (rawchar '_') (rawchar '_') source

    /// Strong emphasis
    let doubleAsterisks (source : Text) : Text = 
        enclose (rawtext "**") (rawtext "**") source

    /// Strong emphasis
    let doubleUnderscores (source : Text) : Text = 
        enclose (rawtext "__") (rawtext "__") source

    /// Backticks for inline code.
    let backticks (source : Text) : Text = 
        enclose (rawchar '`') (rawchar '`') source

    /// Backticks for inline code.
    let doubleBackticks (source : Text) : Text = 
        enclose (rawtext "``") (rawtext "``") source


    let private useReference (altText : string) (identifier : string) : Text = 
        squareBrackets (text altText) ^^ squareBrackets (text identifier)

    /// [Alt text][id]
    let useLinkReference (altText : string) (identifier : string) : Text = 
        hgroup (useReference altText identifier)

    /// ![Alt text][id]
    let useImageReference (altText : string) (identifier : string) : Text = 
        hgroup (bang ^^ useReference altText identifier)
        
    


    let private inlineLinkBody (altText : string) 
                               (path : string) 
                               (title : string option) : Text = 
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
    let inlineLink (altText : string) (path : string) (title : option<string>) : Text = 
        hgroup (inlineLinkBody altText path title) 


    /// ![Alt text](/path/to)
    ///
    /// ![Alt text](/path/to "Title") 
    ///
    /// Note - if path uses backslash as a separator (Windows style) 
    /// it is rewritten to use forward slash (Unix style and Pandoc style).
    /// 
    /// The path should not be explicitly quoted even if it contains spaces.
    let inlineImage (altText : string) 
                    (path : string) 
                    (title : option<string>) : Text = 
        hgroup (bang ^^ inlineLinkBody altText path title) 


    let formatted (fmt : Printf.StringFormat<'a -> string,string>) 
                  (value : 'a) : Text = 
        sprintf fmt value |> text

    /// Make a Text element by replicating the supplied string `count` times.
    let replicated (count : int) (str : string) : Text = 
        String.replicate count str |> rawtext


    /// Print a unsigned byte literal as a decimal.
    /// Note no F# type specifying suffix is printed, if you want this
    /// functionality you need to write your own function.
    let byteMd (i : byte) : Text = 
        i.ToString() |> text
        
    /// Print a signed byte literal as a decimal.
    let sbyteMd (i : sbyte) : Text = 
        i.ToString() |> text

    /// Print a 16-bit signed byte literal as a decimal.
    let int16Md (i : int16) : Text = 
        i.ToString() |> text

    /// Print a 16-bit unsigned byte literal as a decimal.
    let uint16Md (i : uint16) : Text = 
        i.ToString() |> text

    /// Print a 32-bit signed byte literal as a decimal.
    let int32Md (i : int32) : Text = 
        i.ToString() |> text

    /// Print a 32-bit unsigned byte literal as a decimal.
    let uint32Md (i : uint32) : Text = 
        i.ToString() |> text

    /// Print a 64-bit signed byte literal as a decimal.        
    let int64Md (i : int64) : Text = 
        i.ToString() |> text

    /// Print a 64-bit unsigned byte literal as a decimal.
    let uint64Md (i : uint64) : Text = 
        i.ToString() |> text
    
    /// Print a 32-bit IEEE float. 
    /// The output uses FSharp's ToString() so it may be printed in 
    /// exponential notation.
    let float32Md (d : float32) : Text = 
        d.ToString() |> text

    
    /// The output uses FSharp's ToString() so it may be printed in 
    /// exponential notation.
    let doubleMd (d : double) : Text = 
        d.ToString() |> text

    /// The output uses FSharp's ToString().
    let decimalMd (d : decimal) : Text = 
        d.ToString() |> text

    /// Print a DataTime. 
    /// The output uses FSharp's ToString() so it may be printed in 
    /// exponential notation.
    let dateTimeMd (datetime : System.DateTime) (format : string) : Text = 
        datetime.ToString(format) |> text

    /// Format: yyyy-MM-dd hh:mm:ss
    let iso8601DateTimeMd (datetime : System.DateTime) : Text = 
        dateTimeMd datetime "yyyy-MM-dd hh:mm:ss"
    