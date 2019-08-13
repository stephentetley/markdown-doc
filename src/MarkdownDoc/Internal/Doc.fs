// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace MarkdownDoc.Internal


module Doc = 

    open System.Text

    open MarkdownDoc.Internal
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

    /// An interm datatype - it is easier to translate MdText into this than
    /// directly translate to TextElements
    type InterimTextElement = 
        | TE of SimpleDoc.TextElement
        | TNewline

    /// Turn a list of InterimTextElement int a list of Texts 
    /// (a Text represents a line).
    let interimToSimpleText (source : InterimTextElement list) : SimpleText list = 
        List.foldBack (fun elt (currentLine, lines) -> 
                            match elt with
                            | TNewline -> ([], currentLine :: lines)
                            | TE t1 -> (t1 :: currentLine, lines))
                        source
                        ([],[]) 
                |> fun (line1,lines) -> line1 :: lines

    /// Apply Group to multiline text makes each line an unbreakable "image".
    let applyGroup (lines : SimpleText list) : HList<InterimTextElement> = 
        let applyGroup1 (source : SimpleText) : HList<InterimTextElement> = 
            let str1 = source |> List.map (fun v -> v.Content) |>  String.concat "" 
            singletonH (TE (SimpleDoc.TextImage str1))
        List.map applyGroup1 lines |> concatH

    let textToInterim (source : MdText) : InterimTextElement list =
        let rec work t1 cont = 
            match t1 with 
            | EmptyText -> cont emptyH
            | Text s -> 
                cont (singletonH (TE (SimpleDoc.TextString s)))
            | HCatText (t1,t2) ->
                work t1 (fun xs -> 
                work t2 (fun ys -> 
                cont (appendH xs ys)))
            | VCatText (t1,t2) ->
                work t1 (fun xs -> 
                work t2 (fun ys -> 
                cont (appendH xs (consH TNewline ys))))
            | Group t1 -> 
                work t1 (fun xs -> 
                let images = interimToSimpleText (toListH xs) |> applyGroup
                cont images)
        work source (fun x -> x) |> toListH
    
    let textToSimpleText (source : MdText) : SimpleText list = 
        source |> textToInterim |> interimToSimpleText

    let blockToSimpleDoc (source : MdBlock) : SimpleDoc.SimpleDoc = 
        let rec work t1 cont = 
            match t1 with 
            | EmptyBlock -> cont SimpleDoc.Empty
            | BlankLine -> cont (SimpleDoc.Block [])
            | TextBlock txt -> 
                let lines = textToSimpleText txt
                cont (SimpleDoc.Block lines)
            | VCatBlock (b1,b2) ->
                work b1 (fun v1 ->
                work b2 (fun v2 -> 
                cont (VConcat(v1,v2))))

        work source (fun x -> x)