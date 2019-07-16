// Copyright (c) Stephen Tetley 2018,2019
// License: BSD 3 Clause

namespace MarkdownDoc.Markdown

// Explicitly open 

module Table = 
    
    open MarkdownDoc

    type TableCell = ParaElement
    
    type TableRow = TableCell list
