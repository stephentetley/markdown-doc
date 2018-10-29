// Copyright (c) Stephen Tetley 2018
// License: BSD 3 Clause

namespace MarkdownDoc.Internal

open System.Text


[<RequireQualifiedAccess>]
module Tile = 

    /// Maybe a Markdown document is a list of Tiles and tiles don't 
    /// themselves naturally concatenate.

    type Tile = 
        | Tile of string list

        /// Tile can use (+) as it is internal, it's only in the public API 
        /// where we wait to avoid (+).
        static member (+) (a:Tile, b:Tile) = 
            match a,b with
            | Tile(xs), Tile(ys) -> 
                // Separate with an empty line
                let ys1 = "" :: ys in Tile (xs @ ys1)

    let internal getLines (tile:Tile) : string list = 
        match tile with | Tile(xs) -> xs

    let render (tile:Tile) : string = 
        let sb = new StringBuilder()
        List.iter (fun line -> sb.AppendLine(line) |> ignore) <| getLines tile
        sb.ToString()

    let tile (width:int) (text:SimpleText.Text) : Tile = 
        Tile <| SimpleText.renderText width text

    let preformatted (lines:SimpleText.Text list) : Tile = 
        Tile <| List.map SimpleText.renderText1  lines


    let prefixAll (prefix:string) (tile:Tile) : Tile = 
        let lines = getLines tile
        Tile <| List.map (fun line -> prefix + line) lines


    let prefixFirstRest (prefix1:string) (prefix2:string) (tile:Tile) : Tile = 
        let body = 
            match getLines tile with
            | [] -> []
            | line1 :: rest -> (prefix1 + line1) :: List.map (fun line -> prefix2 + line) rest 
        Tile <| body

    /// A each tile is interspersed with a blank line.
    /// If the input list is empty, return an empty Tile.
    let concat (tiles:Tile list) : Tile = 
        match tiles with 
        | [] -> Tile []
        | x :: xs -> List.fold (fun ac b -> ac + b) x xs
        

    // ************************************************************************
    // Tables

    type CellText = string list

    let private gridTableSkeleton (sep1:string) (sep2:string) (sepBody:string) (rows: CellText list) : string list = 
        match rows with
        | [] -> []
        | headings :: body -> 
            let body1 = List.map (fun lines -> lines @ [sepBody]) body
            List.concat <| [[sep1]; headings; [sep2]] @ body1 



    /// The first row is optionallty printed as headers.
    let textGridTable (columnSpecs:ColumnSpec list) (contents: (CellText list) list) 
                        (hasHeaders:bool): Tile = 

        let contentRows = List.map (gridTableRow columnSpecs) contents

        if hasHeaders then 
            let sep1 = gridTableRowSep columnSpecs
            let sep2 = gridTableRowEqualsFormatting columnSpecs
            Tile <| gridTableSkeleton sep1 sep2 sep1 contentRows
        else
            let sep1 = gridTableRowSep columnSpecs
            let sep2 = gridTableRowDashFormatting columnSpecs
            Tile <| gridTableSkeleton sep1 sep2 sep2 contentRows
