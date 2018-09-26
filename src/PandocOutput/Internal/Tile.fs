// Copyright (c) Stephen Tetley 2018
// License: BSD 3 Clause

namespace PandocOutput.Internal

open System.Text


[<RequireQualifiedAccess>]
module Tile = 

    /// Maybe a Markdown document is a list of Tiles and tiles don't 
    /// themselves naturally concatenate.

    type Tile = 
        | Tile of string list
        static member (+) (a:Tile, b:Tile) = 
            match a,b with
            | Tile(xs), Tile(ys) -> Tile (xs @ ys)

    let private getLines (tile:Tile) : string list = 
        match tile with | Tile(xs) -> xs

    let render (tile:Tile) : string = 
        let sb = new StringBuilder()
        List.iter (fun line -> sb.AppendLine(line) |> ignore) <| getLines tile
        sb.ToString()

    let tile (width:int) (text:SimpleText.SimpleText) = 
        Tile <| SimpleText.renderText width text
