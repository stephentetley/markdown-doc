// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace MarkdownDoc.Markdown

[<AutoOpen>]
module Table = 
    
    open MarkdownDoc.Internal
    open MarkdownDoc

    type Alignment = Syntax.Alignment
    type ColumnSpec = Syntax.ColumnSpec
    


    type TableCell = ParaElement
    
    type TableRow = TableCell list

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
