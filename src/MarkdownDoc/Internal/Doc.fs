// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace MarkdownDoc.Internal


module Doc = 

    open System.Text

    open MarkdownDoc.Internal.Common
    open MarkdownDoc.Internal.GridTable
    open MarkdownDoc.Internal.SimpleDoc
    

    /// Note - rendering VCatText writes an explicit Markdown 
    /// line break (two trailing spaces) to the output and 
    /// then a new line
    /// Group changes the render context to render on a single 
    /// unbroken line.
    type MdText =
        private | EmptyText
                | Text of string
                | HCatText of MdText * MdText
                | VCatText of MdText * MdText
                | Group of MdText
        static member empty : MdText = EmptyText


    /// If we include indent information with Blocks we can model code blocks, lists, etc.
    /// without needing specific constructors.
    type Indent = 
        | Uniform of allLines : int
        | Hanging of restLines : int

    
    /// We cannot insist that vertical composition inserts a sapce between blocks with
    /// this representation - e.g. a list is a list of blocks and there is no blank line 
    /// between them.
    type MdBlock = 
        internal | EmptyBlock
                 | BlankLine
                 | TextBlock of MdText
                 | Block of Indent * MdBlock
                 | VCatBlock of MdBlock * MdBlock
                 | TableBlock of ColumnSpec list * MdTableRow option * MdTableRow list 
        static member empty : MdBlock = EmptyBlock
    
    /// We cache width with cell contents so it can be easily accessed
    /// (othwerwise we would have to look up the respective ColumnSpec)
    and MdTableCell = { Width: int; Content: MdBlock }

    and MdTableRow = MdTableCell list


    let textToLines (source : MdText) : Text list =
        let rec work t1 acc cont = 
            match t1 with 
            | EmptyText -> cont acc
            | Text s -> 
                let acc1 = snocH acc [TextString s]
                cont acc1
            //| HCatText of MdText * MdText
            //| VCatText of MdText * MdText
            //| Group of MdText

        work source emptyH (fun x -> x) |> toListH

    let blockToSimpleDoc (source : MdBlock) : SimpleDoc = 
        let rec work t1 cont = 
            match t1 with 
            | EmptyBlock -> cont Empty
        work source (fun x -> x)