// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace MarkdownDoc.Internal


module SimpleDoc = 

    open System.Text

    open MarkdownDoc.Internal.Common
    open MarkdownDoc.Internal.GridTable

    
    
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

    type SimpleText = TextElement list

    /// If we include indent information with Blocks we can model code blocks, lists, etc.
    /// without needing specific constructors.
    type Indent = 
        | Uniform of allLines : string
        | Hanging of firstLine : string * restLines : string


    type SimpleDoc = 
        | Empty 
        | BlankLine
        | Block of lines : SimpleText list
        | Table of columnInfo : ColumnSpec list * headers : SimpleRow option * rows : SimpleRow list 
        | VConcat of SimpleDoc * SimpleDoc
    and SimpleRow = SimpleDoc list

    
    type internal Word = TextElement


    let private textToWords (source : SimpleText) : Word list = 
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
    let breakTextLine (width : int) (source : SimpleText) : string list = 
        let makeLine (xs : Word list) : string = List.rev xs |> wordsToString

        let consWords (words : Word list) (lines : string list) : string list = 
            match words with 
            | [] -> lines
            | _ -> makeLine words :: lines

        let rec work (accLines : string list) 
                     (accWords : Word list) 
                     (pos : int) 
                     (inputs : Word list) 
                     (cont : string list -> string list) =  
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


    let renderSimpleDoc (documentLineWidth : int) (source : SimpleDoc) : string = 
        let rec workDoc (lineWidth : int)
                        (sdoc : SimpleDoc) 
                        (cont : HString -> HString) : HString = 
            match sdoc with
            | Empty -> cont emptyH
            | BlankLine -> cont (singletonH "")
            | Block(lines) -> 
                let xss = List.map (fromListH << breakTextLine lineWidth) lines 
                let acc1 = concatH xss
                cont acc1

            | VConcat(d1,d2) -> 
                workDoc lineWidth d1 (fun xs -> 
                workDoc lineWidth d2 (fun ys -> 
                let acc = appendH xs ys in cont acc ))

            | Table(columnSpecs, None, rows) -> 
                workRows columnSpecs rows (fun xss -> 
                let xss1 = List.map (List.map toStringH) xss
                let (ans : HString) = drawGridTable columnSpecs None xss1
                cont ans)

            | Table(columnSpecs, Some headers, rows) -> 
                workHeader columnSpecs headers (fun hs -> 
                workRows columnSpecs rows (fun xss -> 
                let (hs1 : string list) = List.map toStringH hs
                let (xss1 : (string list) list) = List.map (List.map toStringH) xss
                let (ans : HString) = drawGridTable columnSpecs (Some hs1) xss1
                cont ans))

        and workHeader (specs : ColumnSpec list)
                       (cells : SimpleDoc list)
                       (cont : HString list -> HString) : HString = 
            match specs, cells with
            | [], _ -> cont []
            | _, [] -> cont []
            | (s1 :: srest), (c1 :: crest) -> 
                workDoc s1.Width c1 (fun v1 -> 
                workHeader srest crest (fun vs -> 
                cont (v1 :: vs)))


        and workRows (specs : ColumnSpec list)
                     (rows : (SimpleDoc list) list) 
                     (cont : (HString list) list -> HString) : HString = 
            match rows with
            | [] -> cont []
            | r1 :: rest -> 
                workRow specs r1 (fun v1 -> 
                workRows specs rest (fun vs -> 
                cont (v1 :: vs)))
        
        and workRow (specs : ColumnSpec list)
                    (cells : SimpleDoc list) 
                    (cont : HString list -> HString) : HString = 
            match specs, cells with
            | [], _ -> cont []
            | _, [] -> cont []
            | (s1 :: srest), (c1 :: crest) -> 
                workDoc s1.Width c1 (fun v1 -> 
                workRow srest crest (fun vs -> 
                cont (v1 :: vs)))

        workDoc documentLineWidth source (fun xs -> xs) |> toStringH

    let applyIndent1 (indent1 : string)  (indentRest : string) (source : SimpleText list) : SimpleText list = 
        let indentLine (prefix : string) (txt : SimpleText) = TextString prefix :: txt
        match source with
        | x :: xs -> indentLine indent1 x :: List.map (indentLine indentRest) xs 
        | [] -> []

    let private indentable (source : SimpleDoc) : bool = 
        let rec work sdoc cont = 
            match sdoc with
            | Block xs -> cont (not xs.IsEmpty)
            | VConcat (d1,d2) -> 
                work d1 (fun v1 -> 
                work d2 (fun v2 -> 
                cont (v1 || v2)))
            | _ -> cont false
        work source (fun x -> x)

    let private applyIndentAux (indent1 : string)  (indentRest : string) (source : SimpleDoc) : SimpleDoc = 
        let rec work (sdoc : SimpleDoc) (ind1 : string) (ind2 : string) (cont : SimpleDoc -> SimpleDoc)  = 
            match sdoc with
            | Block lines -> 
                let ans = applyIndent1 ind1 ind2 lines in cont (Block ans)
            | VConcat (d1, d2) when indentable d1 -> 
                work d1 ind1 ind2 (fun v1 -> 
                // use (ind2,ind2) for rest
                work d2 ind2 ind2 (fun v2 -> 
                cont (VConcat(v1,v2))))
            | VConcat (d1,d2) -> 
                // when know d1 cannot be indented
                work d2 ind1 ind2 (fun v2 -> 
                cont (VConcat(d1,v2)))
            | _ -> cont sdoc 
        work source indent1 indentRest (fun x -> x)

    let applyIndent (indent : Indent) (source : SimpleDoc) : SimpleDoc = 
        match indent with
        | Hanging(a,b) -> applyIndentAux a b source
        | Uniform a -> applyIndentAux a a source