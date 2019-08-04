// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace MarkdownDoc.Internal

// Explicitly open all Internal modules.

[<RequireQualifiedAccess>]
module SimpleDoc = 

    open System.Text
    open MarkdownDoc.Internal.Common

    
    type TextElement = 
        | TextString of string
        | TextImage of string        // i.e. an unbreakable string
        | TextSpace 
        member v.Content
            with get() : string = 
                match v with
                | TextString s -> s
                | TextImage s -> s
                | TextSpace -> " "
        member v.Length 
            with get() : int = v.Content.Length

    type internal Text = TextElement list


    type Alignment = 
        | AlignDefault 
        | AlignLeft 
        | AlignCenter 
        | AlignRight

    type ColumnSpec = 
        { Width: int
          Alignment: Alignment }


    type SimpleDoc = 
        | Empty 
        | SimpleBlock of lines : Text list
        | SimpleTable of columnInfo : ColumnSpec list * headers : SimpleDoc option * rows : SimpleDoc list 
    

    
    type internal Word = TextElement


    let private textToWords (source:Text) : Word list = 
        let split1 (text:TextElement) : Word list = 
            match text with
            | TextSpace -> [TextSpace]
            | TextImage(_) -> [text]
            | TextString s -> 
                s.Split(separator= [|' '|]) 
                    |> Array.toList 
                    |> List.map TextString 
                    |> intersperse TextSpace
        source |> List.map split1 |> List.concat


    /// Assumption - textual data was split with on space, thus we can 
    /// use space to join it together
    let internal wordsToString (source:Word list) : string = 
        source |> List.map (fun x -> x.Content) |> String.concat ""


    /// Precondition: source is a single input line with only 
    /// spaces (no tabs/newlines).
    let breakText (width:int) (source:Text) : string list = 
        let makeLine (xs:Word list) : string = List.rev xs |> wordsToString

        let consWords (words:Word list) (lines:string list) : string list = 
            match words with 
            | [] -> lines
            | _ -> makeLine words :: lines

        let rec work (accLines:string list) 
                     (accWords:Word list) (pos:int) (inputs:Word list) 
                     (cont:string list -> string list) =  
            match inputs with 
            | [] -> 
                cont (consWords accWords accLines)
            | (w::ws) -> 
                if pos + 1 + w.Length > width then
                    // The first word encountered might be too long..
                    match accWords with
                    | [] -> 
                        let s1 = makeLine [w] 
                        work (s1 ::accLines) [] 1 ws cont
                    | _ -> work ((makeLine accWords)::accLines) [w] w.Length ws cont
                else 
                    work accLines (w::accWords) (pos + 1 + w.Length) ws cont
        work [] [] 0 (textToWords source) (fun xs -> List.rev xs)



    let renderSimpleDoc (lineWidth : int) (source : SimpleDoc) : string = 
        let rec work sdoc (cont : (string list) list -> string list) = 
            match sdoc with
            | Empty -> cont []
            | SimpleBlock(lines) -> 
                let xss = List.map (breakText lineWidth) lines in cont xss

            //    | SimpleTable of columnInfo : ColumnSpec list * headers : SimpleDoc option * rows : SimpleDoc list 
        work source (fun xss -> List.concat xss) |> fromLines