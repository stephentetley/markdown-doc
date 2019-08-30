// Copyright (c) Stephen Tetley 2018,2019
// License: BSD 3 Clause

#r "netstandard"

#I @"C:\Users\stephen\.nuget\packages\slformat\1.0.2-alpha-20190721\lib\netstandard2.0"
#r "SLFormat.dll"

#load "..\src\MarkdownDoc\Internal\Common.fs"
#load "..\src\MarkdownDoc\Internal\GridTable.fs"
#load "..\src\MarkdownDoc\Internal\SimpleDoc.fs"
#load "..\src\MarkdownDoc\Internal\Doc.fs"
#load "..\src\MarkdownDoc\Markdown\Text.fs"
#load "..\src\MarkdownDoc\Markdown\Block.fs"
#load "..\src\MarkdownDoc\Markdown\Table.fs"
#load "..\src\MarkdownDoc\Extra\InlineHtml.fs"
#load "..\src\MarkdownDoc\Extra\CssColors.fs"
#load "..\src\MarkdownDoc\Extra\InlineHtml.fs"
#load "..\src\MarkdownDoc\Extra\RoseTree.fs"
#load "..\src\MarkdownDoc\Pandoc\Extra.fs"
#load "..\src\MarkdownDoc\Pandoc\Invoke.fs"

open MarkdownDoc.Markdown
open MarkdownDoc.Markdown.InlineHtml
open MarkdownDoc.Markdown.RoseTree
open MarkdownDoc.Pandoc


let test01 () = 
    text "Hello" ^+^ text "world!"
        |> testRenderText 80

let test02 () = 
    let m1 : Markdown = markdownText (text "Hello" ^+^ text "world!")
    testRender 80 m1

let test03 () = 
    let m1 : Markdown = 
        unorderedList [ markdownText <| text "Hello"; markdownText <| text "world!" ]
    testRender 80 m1

let test04 () = 
    let m1 : Markdown = 
        orderedList [ markdownText <| text "Hello"; markdownText <| text "world!" ]
    testRender 80 m1

let fruitColSpecs : ColumnSpec list = 
    [ { Width = 30; Alignment = Alignment.AlignLeft } 
    ; { Width = 40; Alignment = Alignment.AlignCenter } 
    ; { Width = 40; Alignment = Alignment.AlignRight } 
    ]

let test05 () = 
    let plain = markdownText << text
    let headers = [ plain "Fruit"; plain "Price"; plain "Advantages" ]
    let cells = 
        [ [ plain "Bananas"; plain "$1.34"; unorderedList [plain "builtin-in wrapper"; plain "bright color"] ] 
        ; [ plain "Oranges"; plain "$2.10"; unorderedList [plain "cures scurvy"; plain "tasty"] ] 
        ]
    gridTable (makeTable fruitColSpecs headers cells) |> testRender 80


let test06 () = 
    let m1 : Markdown = 
        markdownText (text "hello") ^!!^ openxmlPagebreak ^!!^ markdownText (text "world")
    testRender 80 m1
    

let test07 () = 
    let m1 : Markdown = 
        markdownText <| (text "hello" ^^ text "<world>")
    testRender 80 m1


let test08 () = 
    let m1 : Markdown = 
        markdownText <| textlines [text "hello"; text "world"]
    testRender 80 m1


let test09 () = 
    let m1 : Markdown = 
        markdownText <| inlineLink "" @"d:\This is a very\long\path\that\should prove\rendering\with group\goes on\a single line\README.md" None
    testRender 80 m1


let test10 () = 
    let m1 : Markdown = 
        markdownText <| useImageReference "" "myImage1"
    testRender 80 m1

let test11 () = 
    let ms : Markdown list = 
        [ h1 (rawtext "title1")
        ; h2 (rawtext "title1.1")
        ]
    testRender 80 (vcat ms)
    testRender 80 (vsep ms)

let test12 () = 
    let m1 : Markdown = 
        markdownText <| htmlAnchorIdAttrs "anchor1" [attrTitle "anchor\nline2"] (rawtext "This is an anchor")
    testRender 80 m1


let test13 () = 

    let label (s : string) : Markdown = markdownText (text s)

    let tree1 : RoseTree<Markdown> = 
        makeNode (label "top") 
                 [ makeLeaf (label "one")
                 ; makeLeaf (label "two")
                 ; makeNode (label "three") 
                            [ makeLeaf (label "A")
                            ; makeLeaf (label "B")
                            ]
                 ]
    testRender 80 (drawTree tree1)
    testRender 80 (drawNumberedTree tree1)


