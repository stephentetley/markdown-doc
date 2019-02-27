// Copyright (c) Stephen Tetley 2018,2019
// License: BSD 3 Clause

#r "netstandard"

#I @"C:\Users\stephen\.nuget\packages\slformat\1.0.2-alpha-20190207\lib\netstandard2.0"
#r "SLFormat.dll"

#load "..\src\MarkdownDoc\Internal\Common.fs"
#load "..\src\MarkdownDoc\Internal\Syntax.fs"
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
    let m1 : Markdown = markdownTile (text "Hello" ^+^ text "world!")
    testRender m1

let test03 () = 
    let m1 : Markdown = 
        markdown <| unordList [ paraText <| text "Hello"; paraText <| text "world!" ]
    testRender m1

let test04 () = 
    let m1 : Markdown = 
        markdown <| ordList [ paraText <| text "Hello"; paraText <| text "world!" ]
    testRender m1

let fruitColSpecs : ColumnSpec list = 
    [ { Width = 30; Alignment = Alignment.AlignLeft } 
    ; { Width = 40; Alignment = Alignment.AlignCenter } 
    ; { Width = 40; Alignment = Alignment.AlignRight } 
    ]

let test05 () = 
    let plain = paraText << text
    let headers = [ plain "Fruit"; plain "Price"; plain "Advantages" ]
    let cells = 
        [ [ plain "Bananas"; plain "$1.34"; unordList [plain "builtin-in wrapper"; plain "bright color"] ] 
        ; [ plain "Oranges"; plain "$2.10"; unordList [plain "cures scurvy"; plain "tasty"] ] 
        ]
    gridTable fruitColSpecs (Some headers) cells |> testRender


let test06 () = 
    let m1 : Markdown = 
        markdownTile (text "hello") ^@^ openxmlPagebreak ^@^ markdownTile (text "world")
    testRender m1
    

let test07 () = 
    let m1 : Markdown = 
        markdownTile <| (text "hello" ^^ text "<world>")
    testRender m1


let test08 () = 
    let m1 : Markdown = 
        markdownTile <| textlines [text "hello"; text "world"]
    testRender m1


let test09 () = 
    let m1 : Markdown = 
        markdownTile <| inlineLink "" @"d:\This is a very\long\path\that\should prove\rendering\with group\goes on\a single line\README.md" None
    testRender m1


let test10 () = 
    let m1 : Markdown = 
        markdownTile <| useImageReference "" "myImage1"
    testRender m1


//let test09a () = 
//    Common.breakText 50 (Common.TextualString "hello\r\nworld");;

//let test09b () = 
//    breakline1 10 "ABC DEFGHIJKLMNOP RST UV WXYZ"

//let test09c () = 
//    breaklines 10 "ABC DEFGHIJKLMNOP RST UV WXYZ\n\nABCDEFGHIJKLM NOP RST UV WXYZ"

///// Windows only...
//let test09d () =   "hello\r\nworld".Split(separator=[| System.Environment.NewLine |], options= System.StringSplitOptions.None)





