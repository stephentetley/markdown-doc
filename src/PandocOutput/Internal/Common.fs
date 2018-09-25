// Copyright (c) Stephen Tetley 2018
// License: BSD 3 Clause

namespace PandocOutput.Internal

[<AutoOpen>]
module Common = 


    type Alignment = AlignDefault | AlignLeft | AlignCenter | AlignRight

    
    type ColumnSpec = 
        { Width: int
          Alignment: Alignment }


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

