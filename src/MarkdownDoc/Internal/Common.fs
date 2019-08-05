// Copyright (c) Stephen Tetley 2018,2019
// License: BSD 3 Clause

namespace MarkdownDoc.Internal

// Explicitly open all Internal modules.

module Common = 
    
    open System
    open System.Text

    /// Replace back-slashes with forward-slashes.
    let inline replaceBackslashes (source:string) : string = 
        source.Replace(oldChar = '\\', newChar = '/')

    /// Escape ' ' with "\ "
    let inline escapeSpaces (source:string) : string = 
        source.Replace(oldValue = " ", newValue = "\\ ")


    let internal intersperse (sep:'T) (source:'T list) : 'T list = 
        let rec work (acc:'T list) (xs:'T list) (cont:'T list -> 'T list) = 
            match xs with
            | [] -> cont acc
            | x :: xs -> work (x::sep::acc) xs cont
        match source with
        | [] -> []
        | x :: xs -> work [x] xs (List.rev)




    /// Splits on Environment.NewLine
    let toLines (source:string) : string list = 
        source.Split(separator=[| Environment.NewLine |], options=StringSplitOptions.None) |> Array.toList

    /// Joins with Environment.NewLine
    let fromLines (source:string list) : string = 
        String.concat Environment.NewLine source

    /// "Enclose" and concatenate a list of strings.
    /// The same separator is used for the interior separator
    /// and the bookends.
    let encloseConcat (separator:string) (items:string list) : string = 
        let sb = new StringBuilder(separator)
        List.iter (fun (item:string) -> 
                        sb.Append(item) |> ignore
                        sb.Append(separator) |> ignore) items
        sb.ToString()


    /// FSharp's builtin List.map2 (aka zipWith) throws an exception if one list is shorter than the other.
    /// We prefer truncation instead.
    let raggedMap2 (mapping:'a -> 'b -> 'c) (list1:'a list) (list2:'b list) : 'c list = 
        let rec work acc ls1 ls2 cont = 
            match ls1,ls2 with 
            | [], _ -> cont acc
            | _, [] -> cont acc
            | x::xs, y::ys -> 
                work ((mapping x y)::acc) xs ys cont
        work [] list1 list2 (List.rev)

    /// F#'s built-in List.transpose needs perfect input. It cannot handle ragged tables.
    let raggedTransposeRow (row:(string list) list) : (string list) list = 
        let headsOf (cells:(string list) list) : string list = 
            List.map (fun xs -> match xs with | [] -> ""; | (x::_) -> x) cells
        let tailsOf (cells:(string list) list) : (string list) list = 
            List.map (fun xs -> match xs with | (_::ys) -> ys; | _ -> []) cells
        
        let rec work xs cont = 
            match xs with
            | [] -> cont []
            | rows when List.forall List.isEmpty xs -> cont []
            | rows ->
                let line1 = headsOf rows
                let rest = tailsOf rows
                work rest (fun vs -> 
                cont (line1::vs))
        work row (fun x -> x)

    
    // ************************************************************************
    // Breaking lines when rendering.
    // We want output to be generally nice looking. i.e
    // paragraphs formatted to lines 80 chars or so except 
    // where the line has unbreakable text like image URLs.


    type internal TextualData1 = 
        | TextualString of string
        | TextualImage of string        // i.e. an unbreakable string
        | TextualSpace 
        member v.Content
            with get() : string = 
                match v with
                | TextualString s -> s
                | TextualImage s -> s
                | TextualSpace -> " "
        member v.Length 
            with get() : int = v.Content.Length

    type internal TextualData = TextualData1 list

    type internal Word = TextualData1

    let private textualToWords (source:TextualData) : Word list = 
        let split1 (text:TextualData1) = 
            match text with
                | TextualSpace -> [TextualSpace]
                | TextualImage(_) -> [text]
                | TextualString s -> 
                    s.Split(separator= [|' '|]) 
                        |> Array.toList 
                        |> List.map TextualString 
                        |> intersperse TextualSpace
        source |> List.map split1 |> List.concat


    /// Assumption - textual data was split with on space, thus we can 
    /// use space to join it together
    let internal wordsToString (source:Word list) : string = 
        source |> List.map (fun x -> x.Content) |> String.concat ""


    /// Precondition: source is a single input line with only 
    /// spaces (no tabs/newlines).
    let breakText (width:int) (source:TextualData) : string list = 
        let makeLine (xs:Word list) : string = List.rev xs |> wordsToString
    
        let consWords (words:TextualData1 list) (lines:string list) : string list = 
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
        work [] [] 0 (textualToWords source) (fun xs -> List.rev xs)


    // ************************************************************************

    type HString = string list -> string list

    let emptyH  : HString = id

    let consH (item : string) (s1 : HString) : HString = (fun x -> item :: x) << s1

    let singletonH (item : string) : HString = consH item emptyH 

    let snocH (s1 : HString) (item : string) : HString = s1 << (fun x -> item :: x) 
    
    let appendH (s1 : HString) (s2 : HString) : HString = s1 << s2

    let concatH (strs : HString list) : HString = List.foldBack (<<) strs id
    
    let toListH (s1 : HString) : string list = s1 []

    let fromListH (xs : string list) : HString = fun ys -> xs @ ys


