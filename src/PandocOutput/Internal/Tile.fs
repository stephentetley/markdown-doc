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


    let prefixAll (prefix:string) (tile:Tile) : Tile = 
        let lines = getLines tile
        Tile <| List.map (fun line -> prefix + line) lines


    let prefixFirstRest (prefix1:string) (prefix2:string) (tile:Tile) : Tile = 
        let body = 
            match getLines tile with
            | [] -> []
            | line1 :: rest -> (prefix1 + line1) :: List.map (fun line -> prefix2 + line) rest 
        Tile <| body

    let concat (tiles:Tile list) : Tile = 
        Tile <| List.concat (List.map getLines tiles)


