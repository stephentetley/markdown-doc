﻿// Copyright (c) Stephen Tetley 2018
// License: BSD 3 Clause

namespace MarkdownDoc.Internal

open System.Text

[<AutoOpen>]
module Common = 

    
    let encloseConcat (separator:string) (items:string list) : string = 
        let sb = new StringBuilder(separator)
        List.iter (fun (item:string) -> 
                        sb.Append(item) |> ignore
                        sb.Append(separator) |> ignore) items
        sb.ToString()

    /// F#'s built-in List.transpose needs perfect input. It cannot handle ragged tables.
    let raggedTranspose (emptyElement:'a) (table:('a list) list) : ('a list) list = 
        let headsOf (table:('a list) list) : 'a list = 
            List.map (fun xs -> match xs with | [] -> emptyElement; | (x::_) -> x) table
        let tailsOf (table:('a list) list) : ('a list) list = 
            List.map (fun xs -> match xs with | (_::ys) -> ys; | _ -> []) table
        let rec work ac rows = 
            if List.forall (fun (x:'a list)  -> List.isEmpty x) rows then 
                List.rev ac
            else 
                let line1 = headsOf rows
                let rest = tailsOf rows
                work (line1::ac) rest
        work [] table

    // ************************************************************************
    // Tables

    type Alignment = AlignDefault | AlignLeft | AlignCenter | AlignRight

    
    type ColumnSpec = 
        { Width: int
          Alignment: Alignment }


    

    /// Note the printed column width is two characters wider than the 
    /// width in the specification. This accounts for left and right spacing 
    /// when cells are printed.
    let gridTableRowSep (specs:ColumnSpec list) : string = 
        encloseConcat "+" 
            <| List.map (fun spec -> String.replicate (spec.Width + 2) "-") specs

    /// Cell is two characters wider than the specification to allow for left
    /// and right spacing.
    let cellSpecifier (width:int) (align:Alignment) (ch:char) : string = 
        let chs = ch.ToString()
        match align with
        | AlignDefault -> String.replicate (width+2) chs
        | AlignLeft -> ":" + String.replicate (width+1) chs
        | AlignCenter -> ":" + String.replicate width chs + ":"
        | AlignRight -> String.replicate (width+1) chs + ":"

    let private gridTableRowSepWithFormatting (ch:char) (specs:ColumnSpec list) : string = 
        encloseConcat "+" 
            <| List.map (fun spec -> cellSpecifier spec.Width spec.Alignment ch) specs

    let gridTableRowDashFormatting (specs:ColumnSpec list) : string = 
        gridTableRowSepWithFormatting '-' specs

    let gridTableRowEqualsFormatting (specs:ColumnSpec list) : string = 
        gridTableRowSepWithFormatting '=' specs

    type CellContent = string list


    let gridTableContentRow (specs:ColumnSpec list) (texts:string list) : string = 
        // note the cell is 2+spec width to account for left and right spacing
        let padCell (spec:ColumnSpec) (text:string) = 
            " " + text.PadRight(spec.Width + 1 , ' ')

        List.map2 padCell specs texts |> encloseConcat "|"
        
    let gridTableRow (columnSpecs:ColumnSpec list) (cells:CellContent list) : string list = 
        let listsOfLines = raggedTranspose "" cells
        List.map (gridTableContentRow columnSpecs) listsOfLines


    type Lines = string list
    type Words = string list



    /// Precondition: source is a single line with only spaces (no tabs/newlines)
    let breakline1 (width:int) (source:string) : string list = 
        let words = source.Split(' ') |> Array.toList
        let makeLine (words:Words) : string = String.concat " " (List.rev words)
        let rec work (acc:Lines) (a1:Words) (pos:int) (ss:Words) =  
            match ss with 
            | [] -> 
                if List.isEmpty a1 then 
                    List.rev acc 
                else List.rev ((makeLine a1)::acc)
            | (w::ws) -> 
                if pos + 1 + w.Length > width then
                    // The first word encountered might be too long..
                    if List.isEmpty a1 then 
                        work (w::acc) [] 1 ws
                    else
                        work ((makeLine a1)::acc) [w] w.Length ws
                else 
                    work acc (w::a1) (pos + 1 + w.Length) ws
        work [] [] 0 words

    let breaklines (width:int) (source:string) : string list = 
        let lines = source.Split([| '\n' |]) 
        Array.map (breakline1 width) lines |> List.concat 
