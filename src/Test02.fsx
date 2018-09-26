// Copyright (c) Stephen Tetley 2018
// License: BSD 3 Clause

#load "PandocOutput\Internal\Common.fs"
#load "PandocOutput\Internal\SimpleText.fs"
#load "PandocOutput\Internal\Tile.fs"
#load "PandocOutput\Markdown.fs"



open System.IO
open System.Text
open PandocOutput


let test01 () = 
    rawtext "Hello" <+> rawtext "world!"
        |> testRenderText

let test02 () = 
    let m1 : Markdown = tile (rawtext "Hello" <+> rawtext "world!")
    testRender m1

//let test02 () = 
//    plaintext "hello world" + plaintext "!"
//        |> testRender

//let test03 () = 
//    docxPagebreak
//        |> testRender



//let test04 () = 
//    breakline1 20 "The quick brown fox jumped over the lazy dog." 
//    // "hello world".Split(' ')

//let test05 () = 
//    breakline1 10 "ABCDEFGHIJKLM NOP RST UV WXYZ"

//let test06 () = 
//    breakline1 10 "ABC DEFGHIJKLMNOP RST UV WXYZ"

//let test07 () = 
//    breaklines 10 "ABC DEFGHIJKLMNOP RST UV WXYZ\n\nABCDEFGHIJKLM NOP RST UV WXYZ"


//let fruitColSpecs : ColumnSpec list = 
//    [ { Width = 30; Alignment = AlignDefault } 
//    ; { Width = 40; Alignment = AlignDefault } 
//    ; { Width = 40; Alignment = AlignDefault } 
//    ]

//let test08 () = 
//    gridTableRegularSep fruitColSpecs





//let test09 () = 
//    let cells = 
//        [ [ plaintext "Fruit"; plaintext "Price"; plaintext "Advantages" ]
//        ; [ plaintext "Bananas"; plaintext "$1.34"; unordList [plaintext "builtin-in wrapper"; plaintext "bright color"] ] 
//        ; [ plaintext "Oranges"; plaintext "$2.10"; unordList [plaintext "cures scurvy"; plaintext "tasty"] ] 
//        ]
//    gridTable fruitColSpecs cells |> testRender



//let test10 () = 
//    blockquote (plaintext "Hollow" @@@ plaintext "world!") 
//        |> testRender

