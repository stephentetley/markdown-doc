// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace MarkdownDoc.Internal


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
        | Block of lines : Text list
        | Table of columnInfo : ColumnSpec list * headers : SimpleRow option * rows : SimpleRow list 
        | VConcat of SimpleDoc * SimpleDoc
    and SimpleRow = SimpleDoc list

    
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
    let breakTextLine (width:int) (source:Text) : string list = 
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
    


    //let renderSimpleRow (row : SimpleRow) : string list = 
    //    let rec workCells (acc:string list) 
    //                      (cells : SimpleRow list) 
    //                      (cont:string list -> string list) = 
    //        match cells with
    //        | [] -> cont (List.rev acc)
    //        | c1 :: cs -> 
    //            let str1 = renderMdParaElement c1.Width c1.Content
    //            workCells (str1::acc) cs cont
    //    workCells [] row id

    let renderSimpleDoc (lineWidth : int) (source : SimpleDoc) : string = 
        let rec workDoc (sdoc : SimpleDoc) 
                        (cont : string list -> string list) = 
            match sdoc with
            | Empty -> cont []
            | Block(lines) -> 
                let xss = List.map (breakTextLine lineWidth) lines 
                let acc1 = List.concat xss
                cont acc1
            | VConcat(Empty,d2) -> 
                workDoc d2 cont
            | VConcat(d1,Empty) -> 
                workDoc d1 cont
            | VConcat(d1,d2) -> 
                workDoc d1 (fun xs -> 
                workDoc d2 (fun ys -> 
                let acc = xs @ [""] @ ys in cont acc ))
            // | Table(columnSpecs,header,rows) -> 
                
            //    /// Send a partially instantiated table-text building function to `WorkRows`
            //    let tableToString rows =
            //         textGridTable columnSpecs (Option.map renderTableRow1 header) rows
            //    workRows tableToString [] rows (fun acc1 -> 
            //    let tableText = acc1.ToString()
            //    cont (acc.AppendLine(tableText)))
        //and workRows (makeTableText:(string list) list -> string)
        //             (acc:(string list) list) 
        //             (rows : MdTableRow list) 
        //             (cont:StringBuilder -> string) : string = 
        //    match rows with
        //    | [] -> let tableText = makeTableText (List.rev acc)
        //            cont (new StringBuilder(value=tableText))
        //    | x :: xs -> 
        //        let rowCells = renderTableRow1 x 
        //        workRows makeTableText (rowCells::acc) xs cont

            //    | SimpleTable of columnInfo : ColumnSpec list * headers : SimpleDoc option * rows : SimpleDoc list 


        workDoc source (fun xs -> xs) |> fromLines