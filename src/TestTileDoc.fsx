// Copyright (c) Stephen Tetley 2018
// License: BSD 3 Clause


#load "PandocOutput\Internal\Common.fs"
#load "PandocOutput\Internal\TileDoc.fs"

open PandocOutput.Internal
open PandocOutput.Internal.TileDoc




let test01 () = 
    doubleUnderscores (rawtext "Hollow" <+> rawtext "world!") 
        |> renderText 40 


let test02 () = 
    let line1 = doubleUnderscores (rawtext "Hollow" <+> rawtext "world!")
    let line2 = rawtext "-----------------"
    tile line1 + tile line2 
        |> codeBlock
        |> render
        |> printfn "%s"

let fruitColSpecs : ColumnSpec list = 
    [ { Width = 28; Alignment = AlignDefault } 
    ; { Width = 38; Alignment = AlignDefault } 
    ; { Width = 38; Alignment = AlignDefault } 
    ]

let temp01 () = 
    printfn "%s" <| gridTableRowSep fruitColSpecs
    printfn "%s" <| gridTableRowEqualsFormatting fruitColSpecs

let test03 (hasHeaders:bool) : unit = 
    let cells = 
        [ [ rawtext "Fruit"; rawtext "Price"; rawtext "Advantages" ]
        ; [ rawtext "Bananas"; rawtext "$1.34"; rawtext "**We need Tiles for lists**" ]
        ; [ rawtext "Oranges"; rawtext "$2.10"; rawtext "**We need Tiles for lists.** This implies rendering tile width should be contextual so we can get it from the ColumnSpec." ]  
        ]
    textGridTable fruitColSpecs cells  hasHeaders       
        |> render
        |> printfn "%s"

let test04 () : unit = 
    ordList <| List.map (tile << rawtext) ["Bird"; "McHale"; "Parish"]
        |> render
        |> printfn "%s"


let test05 () : unit = 
    unordList <| List.map (tileW 35 << rawtext) ["Bird"; "McHale"; "Parish (this is extended text that should continue on to the next line)."]
        |> render
        |> printfn "%s"