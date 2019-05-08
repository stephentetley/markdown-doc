// Copyright (c) Stephen Tetley 2018,2019
// License: BSD 3 Clause

#r "netstandard"

#I @"C:\Users\stephen\.nuget\packages\slformat\1.0.2-alpha-20190322\lib\netstandard2.0"
#r "SLFormat.dll"

#load "..\src\MarkdownDoc\Internal\Common.fs"
#load "..\src\MarkdownDoc\Internal\Syntax.fs"
#load "..\src\MarkdownDoc\Markdown.fs"
#load "..\src\MarkdownDoc\Pandoc\Extra.fs"
#load "..\src\MarkdownDoc\Pandoc\Invoke.fs"

open MarkdownDoc
open MarkdownDoc.Pandoc


let test01 () = 
    text "Hello" ^+^ text "world!"
        |> testRenderText

let test02 () = 
    let m1 : Markdown = markdownText (text "Hello" ^+^ text "world!")
    testRender m1

let test03 () = 
    let m1 : Markdown = 
        markdown <| unorderedList [ paraText <| text "Hello"; paraText <| text "world!" ]
    testRender m1

let test04 () = 
    let m1 : Markdown = 
        markdown <| orderedList [ paraText <| text "Hello"; paraText <| text "world!" ]
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
        [ [ plain "Bananas"; plain "$1.34"; unorderedList [plain "builtin-in wrapper"; plain "bright color"] ] 
        ; [ plain "Oranges"; plain "$2.10"; unorderedList [plain "cures scurvy"; plain "tasty"] ] 
        ]
    gridTable fruitColSpecs (Some headers) cells |> testRender


let test06 () = 
    let m1 : Markdown = 
        markdownText (text "hello") ^@^ openxmlPagebreak ^@^ markdownText (text "world")
    testRender m1
    

let test07 () = 
    let m1 : Markdown = 
        markdownText <| (text "hello" ^^ text "<world>")
    testRender m1


let test08 () = 
    let m1 : Markdown = 
        markdownText <| textlines [text "hello"; text "world"]
    testRender m1


let test09 () = 
    let m1 : Markdown = 
        markdownText <| inlineLink "" @"d:\This is a very\long\path\that\should prove\rendering\with group\goes on\a single line\README.md" None
    testRender m1


let test10 () = 
    let m1 : Markdown = 
        markdownText <| useImageReference "" "myImage1"
    testRender m1






