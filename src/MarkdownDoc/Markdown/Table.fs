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
                  (rows : TableRow list) = 
        { ColumnSpecs = columnSpecs
          ColumnHeadings = Some columnHeadings
          Rows = rows
        }
