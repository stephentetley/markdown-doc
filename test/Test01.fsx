// Copyright (c) Stephen Tetley 2018
// License: BSD 3 Clause

#load "..\src\MarkdownDoc\Internal\Common.fs"
#load "..\src\MarkdownDoc\Internal\SimpleText.fs"
#load "..\src\MarkdownDoc\Internal\Tile.fs"
#load "..\src\MarkdownDoc\Markdown.fs"



open System.IO
open System.Text
open MarkdownDoc


let test01 () = 
    rawtext "Hello" <+> rawtext "world!"
        |> testRenderText

let test02 () = 
    let m1 : Markdown = tile (rawtext "Hello" <+> rawtext "world!")
    testRender m1

let test03 () = 
    let m1 : Markdown = 
        unordList [ tile <| rawtext "Hello"; tile <| rawtext "world!" ]
    testRender m1

let test04 () = 
    let m1 : Markdown = 
        ordList [ tile <| rawtext "Hello"; tile <| rawtext "world!" ]
    testRender m1

let fruitColSpecs : ColumnSpec list = 
    [ { Width = 30; Alignment = Alignment.AlignDefault } 
    ; { Width = 40; Alignment = Alignment.AlignDefault } 
    ; { Width = 40; Alignment = Alignment.AlignDefault } 
    ]

let test05 () = 
    let cells = 
        let plain = tile << rawtext
        [ [ plain "Fruit"; plain "Price"; plain "Advantages" ]
        ; [ plain "Bananas"; plain "$1.34"; unordList [plain "builtin-in wrapper"; plain "bright color"] ] 
        ; [ plain "Oranges"; plain "$2.10"; unordList [plain "cures scurvy"; plain "tasty"] ] 
        ]
    gridTable fruitColSpecs cells true |> testRender


//let test04 () = 
//    breakline1 20 "The quick brown fox jumped over the lazy dog." 
//    // "hello world".Split(' ')

//let test05 () = 
//    breakline1 10 "ABCDEFGHIJKLM NOP RST UV WXYZ"

//let test06 () = 
//    breakline1 10 "ABC DEFGHIJKLMNOP RST UV WXYZ"

//let test07 () = 
//    breaklines 10 "ABC DEFGHIJKLMNOP RST UV WXYZ\n\nABCDEFGHIJKLM NOP RST UV WXYZ"







//let test10 () = 
//    blockquote (plaintext "Hollow" @@@ plaintext "world!") 
//        |> testRender

