// Copyright (c) Stephen Tetley 2018
// License: BSD 3 Clause

#load "PandocOutput\Internal\Common.fs"
#load "PandocOutput\Internal\FormatCombinators.fs"
#load "PandocOutput\Markdown.fs"
#load "PandocOutput\GridTable.fs"

open PandocOutput
open System.IO
open System.Text
open PandocOutput.Internal
open PandocOutput.Internal


let test01 () = 
    raw2 "haskell" @"qsort [] = []" 
        |> testRender


let test02 () = 
    plaintext "hello world" + plaintext "!"
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


let fruitSpecs : ColumnSpec list = 
    [ { Width = 30; Alignment = AlignDefault } 
    ; { Width = 40; Alignment = AlignDefault } 
    ; { Width = 40; Alignment = AlignDefault } 
    ]

let test08 () = 
    gridTableRegularSep fruitSpecs





let test09 () = 
    let cells = 
        [ [ plaintext "Fruit"; plaintext "Price"; plaintext "Advantages" ]
        ; [ plaintext "Bananas"; plaintext "$1.34"; unordList [plaintext "builtin-in wrapper"; plaintext "bright color"] ] 
        ; [ plaintext "Oranges"; plaintext "$2.10"; unordList [plaintext "cures scurvy"; plaintext "tasty"] ] 
        ]
    simpleTable fruitSpecs cells |> testRender



let test10 () = 
    blockquote (plaintext "Hollow" @@@ plaintext "world!") 
        |> testRender
