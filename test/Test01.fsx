// Copyright (c) Stephen Tetley 2018
// License: BSD 3 Clause

#load "..\src\MarkdownDoc\Internal\Common.fs"
#load "..\src\MarkdownDoc\Internal\MarkdownText.fs"
#load "..\src\MarkdownDoc\Internal\MarkdownTile.fs"
#load "..\src\MarkdownDoc\Markdown.fs"
#load "..\src\MarkdownDoc\Pandoc\Extra.fs"
#load "..\src\MarkdownDoc\Pandoc\Invoke.fs"

open MarkdownDoc
open MarkdownDoc.Internal
open MarkdownDoc.Internal.Common
open MarkdownDoc.Pandoc


let test01 () = 
    text "Hello" ^+^ text "world!"
        |> testRenderText

let test02 () = 
    let m1 : Markdown = tile (text "Hello" ^+^ text "world!")
    testRender m1

let test03 () = 
    let m1 : Markdown = 
        unordList [ tile <| text "Hello"; tile <| text "world!" ]
    testRender m1

let test04 () = 
    let m1 : Markdown = 
        ordList [ tile <| text "Hello"; tile <| text "world!" ]
    testRender m1

let fruitColSpecs : ColumnSpec list = 
    [ { Width = 30; Alignment = Alignment.AlignDefault } 
    ; { Width = 40; Alignment = Alignment.AlignDefault } 
    ; { Width = 40; Alignment = Alignment.AlignDefault } 
    ]

let test05 () = 
    let cells = 
        let plain = tile << text
        [ [ plain "Fruit"; plain "Price"; plain "Advantages" ]
        ; [ plain "Bananas"; plain "$1.34"; unordList [plain "builtin-in wrapper"; plain "bright color"] ] 
        ; [ plain "Oranges"; plain "$2.10"; unordList [plain "cures scurvy"; plain "tasty"] ] 
        ]
    gridTable fruitColSpecs cells true |> testRender


let test06 () = 
    let m1 : Markdown = 
        tile (text "hello") ^@^ openxmlPagebreak ^@^ tile (text "world")
    testRender m1
    

let test07 () = 
    let m1 : Markdown = 
        tile <| (text "hello" ^^ text "<world>")
    testRender m1


let test08 () = 
    let m1 : Markdown = 
        tile <| textlines [text "hello"; text "world"]
    testRender m1

let test08a () = 
    MarkdownText.renderBounded 50 <| textlines [text "hello"; text "world"]

let test08b () = 
    MarkdownText.renderUnbound <| textlines [text "hello"; text "world"]

let test08c () = 
    Common.breaklines 50 "hello\r\nworld" ;;

//let test06 () = 
//    breakline1 10 "ABC DEFGHIJKLMNOP RST UV WXYZ"

//let test07 () = 
//    breaklines 10 "ABC DEFGHIJKLMNOP RST UV WXYZ\n\nABCDEFGHIJKLM NOP RST UV WXYZ"

/// Windows only...
let test09 () =   "hello\r\nworld".Split(separator=[| System.Environment.NewLine |], options= System.StringSplitOptions.None)

let test10 () = 
    MarkdownText.renderBounded 50 << MarkdownText.stringText <| Common.fromLines ["hello";"world"]


