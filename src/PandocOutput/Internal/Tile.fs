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

    let tile (width:int) (text:SimpleText.Text) = 
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

    let private gridTableSkeleton (sep1:string) (sep2:string) (sepBody:string) (rows: (string list) list) : string list = 
        match rows with
        | [] -> []
        | headings :: body -> 
            let body1 = List.map (fun lines -> lines @ [sepBody]) body
            List.concat <| [[sep1]; headings; [sep2]] @ body1 

    /// The first row is printed as headers.
    let textGridTable (columnSpecs:ColumnSpec list) (contents: (SimpleText.Text list) list) 
                        (hasHeaders:bool): Tile = 
        let tileCell (spec:ColumnSpec) (cellText:SimpleText.Text) : string list = 
            SimpleText.renderText spec.Width cellText

        let tileRow (columnSpecs:ColumnSpec list) (cells: SimpleText.Text list) : (string list) list = 
            List.map2 tileCell columnSpecs cells
    
        let listOfRows : CellContent list list = List.map (tileRow columnSpecs) contents

        let contentRows = List.map (gridTableRow columnSpecs) listOfRows

        if hasHeaders then 
            let sep1 = gridTableRowSep columnSpecs
            let sep2 = gridTableRowEqualsFormatting columnSpecs
            Tile <| gridTableSkeleton sep1 sep2 sep1 contentRows
        else
            let sep1 = gridTableRowSep columnSpecs
            let sep2 = gridTableRowDashFormatting columnSpecs
            Tile <| gridTableSkeleton sep1 sep2 sep2 contentRows
