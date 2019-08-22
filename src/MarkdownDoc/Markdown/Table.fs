// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace MarkdownDoc.Markdown

[<AutoOpen>]
module Table = 
    
    open MarkdownDoc.Internal


    type Alignment = GridTable.Alignment
    type ColumnSpec = GridTable.ColumnSpec
    


    type TableCell = Doc.MdTableCell
    
    type TableRow = Doc.MdTableRow

    type Table = 
        { ColumnSpecs : ColumnSpec list
          ColumnHeadings : TableRow option
          Rows : TableRow list
        }

    let makeTable (columnSpecs : ColumnSpec list)
                  (columnHeadings : TableRow)
                  (rows : TableRow list) : Table = 
        { ColumnSpecs = columnSpecs
          ColumnHeadings = Some columnHeadings
          Rows = rows
        }

    // ************************************************************************
    // Alternative contruction

    type ColumnHeading = 
        { ColumnName : Markdown
          ColumnFormatting : ColumnSpec 
        }

    type ColumnHeadings = ColumnHeading list

    let alignDefault (width : int) (columnName : Markdown) : ColumnHeading = 
        let spec = { ColumnSpec.Width = width
                   ; ColumnSpec.Alignment = Alignment.AlignDefault }
        { ColumnName = columnName; ColumnFormatting = spec }

    let alignLeft (width : int) (columnName : Markdown) : ColumnHeading = 
        let spec = { ColumnSpec.Width = width
                   ; ColumnSpec.Alignment = Alignment.AlignLeft }
        { ColumnName = columnName; ColumnFormatting = spec }


    let alignCenter (width : int) (columnName : Markdown) : ColumnHeading = 
        let spec = { ColumnSpec.Width = width
                   ; ColumnSpec.Alignment = Alignment.AlignCenter }
        { ColumnName = columnName; ColumnFormatting = spec }

    let alignRight (width : int) (columnName : Markdown) : ColumnHeading = 
        let spec = { ColumnSpec.Width = width
                   ; ColumnSpec.Alignment = Alignment.AlignRight }
        { ColumnName = columnName; ColumnFormatting = spec }



    let makeTableWithHeadings (headings : ColumnHeadings) 
                              (rows : TableRow list) : Table = 
        let colSpecs = headings |> List.map (fun x -> x.ColumnFormatting)
        let colNames = headings |> List.map (fun x -> x.ColumnName)
        makeTable colSpecs colNames rows
