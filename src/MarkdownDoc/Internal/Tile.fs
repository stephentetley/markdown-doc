// Copyright (c) Stephen Tetley 2018
// License: BSD 3 Clause

namespace MarkdownDoc.Internal.Tile




[<RequireQualifiedAccess>]
module Tile = 
    open System.IO
    open System.Text

    open MarkdownDoc.Internal
    open MarkdownDoc.Internal.Common

    /// Maybe a Markdown document is a list of Tiles and tiles don't 
    /// themselves naturally concatenate.

    type Tile = 
        | Tile of string list

        member internal x.TextLines 
            with get() = match x with | Tile(xs) -> xs

        /// Tile can use (+) as it is internal, it's only in the public API 
        /// where we want to avoid (+) and using (+) avoids taking another 
        /// operator name.
        static member (+) (a:Tile, b:Tile) = 
            match a,b with
            | Tile(xs), Tile(ys) -> 
                // Separate with an empty line
                let ys1 = "" :: ys in Tile (xs @ ys1)
        
        member x.SaveToString() = 
            let sb = new StringBuilder()
            List.iter (fun line -> sb.AppendLine(line) |> ignore) <| x.TextLines
            sb.ToString()

        member x.Save (sw:StreamWriter) = 
            List.iter (fun (line:string) -> sw.WriteLine(line)) x.TextLines

    //let render (tile:Tile) : string = 
    //    let sb = new StringBuilder()
    //    List.iter (fun line -> sb.AppendLine(line) |> ignore) <| tile.TextLines
    //    sb.ToString()

    let tile (width:int) (text:SimpleText.Text) : Tile = 
        Tile <| SimpleText.renderText width text

    let preformatted (lines:SimpleText.Text list) : Tile = 
        Tile <| List.map SimpleText.renderText1  lines


    let prefixAll (prefix:string) (tile:Tile) : Tile = 
        Tile <| List.map (fun line -> prefix + line) tile.TextLines


    let prefixFirstRest (prefix1:string) (prefix2:string) (tile:Tile) : Tile = 
        let body = 
            match tile.TextLines with
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
