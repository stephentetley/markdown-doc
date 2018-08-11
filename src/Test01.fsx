// Copyright (c) Stephen Tetley 2018
// License: BSD 3 Clause

#load @"PandocOutput\Internal\FormatCombinators.fs"
#load @"PandocOutput\Markdown.fs"

open PandocOutput.Markdown
open PandocOutput.Markdown.GridTableHelpers
open System.IO
open System.Text


let test01 () = 
    raw2 "haskell" @"qsort [] = []" 
        |> testRender


let test02 () = 
    plaintext "hello world" +++ plaintext "!"
        |> testRender

let test03 () = 
    docxPagebreak
        |> testRender



let test04 () = 
    breakline1 20 "The quick brown fox jumped over the lazy dog." 
    // "hello world".Split(' ')

let test05 () = 
    breakline1 10 "ABCDEFGHIJKLM NOP RST UV WXYZ"

let test06 () = 
    breakline1 10 "ABC DEFGHIJKLMNOP RST UV WXYZ"

let test07 () = 
    breaklines 10 "ABC DEFGHIJKLMNOP RST UV WXYZ\n\nABCDEFGHIJKLM NOP RST UV WXYZ"

let gridTableLineSep (cols:int list) : string = 
    let sb = new StringBuilder("+")
    let rec work (widths:int list) = 
        match widths with 
        | [] -> sb.ToString()
        | (w :: ws) ->
            sb.Append(String.replicate w "-") |> ignore
            sb.Append('+') |> ignore
            work ws
    work cols


let column1 (width:int) (align:Alignment) (ch:char) : string = 
    match align with
    | AlignDefault -> String.replicate width (ch.ToString())
    | AlignLeft -> ":" + String.replicate (width-1) (ch.ToString())
    | AlignCenter -> ":" + String.replicate (width-2) (ch.ToString()) + ":"
    | AlignRight -> String.replicate (width-1) (ch.ToString()) + ":"
