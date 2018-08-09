// Copyright (c) Stephen Tetley 2018
// License: BSD 3 Clause

#load @"PandocOutput\Internal\FormatCombinators.fs"
#load @"PandocOutput\Markdown.fs"

open PandocOutput.Markdown


let test01 () = 
    raw2 "haskell" @"qsort [] = []" 
        |> testRender


let test02 () = 
    plaintext "hello world" +++ plaintext "!"
        |> testRender

let test03 () = 
    docxPagebreak
        |> testRender

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

let test04 () = 
    breakline1 20 "The quick brown fox jumped over the lazy dog." 
    // "hello world".Split(' ')

let test05 () = 
    breakline1 10 "ABCDEFGHIJKLM NOP RST UV WXYZ" ;;

let test06 () = 
    breakline1 10 "ABC DEFGHIJKLMNOP RST UV WXYZ" ;;
