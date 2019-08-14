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
        internal | EmptyText
                 | Text of string
                 | HCatText of MdText * MdText
                 | VCatText of MdText * MdText
                 | Group of MdText


    


    
    /// We cannot insist that vertical composition inserts a space between blocks with
    /// this representation - e.g. a list is a list of blocks and there is no blank line 
    /// between them.
    /// Vertical spacing must be done "manually" though the combinators should eliminate
    /// concatenations of Blank and Blank   .
    type MdBlock = 
        internal | EmptyBlock
                 | BlankLine
                 | TextBlock of MdText
                 | Block of Indent * MdBlock
                 | VCatBlock of MdBlock * MdBlock
                 | TableBlock of ColumnSpec list * MdTableRow option * MdTableRow list 
    
    /// Cell is just a Block
    and MdTableCell = MdBlock

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
                cont (SimpleDoc.VConcat(v1,v2))))
            | Block (indent, b1) -> 
                work b1 (fun v1 -> 
                let v2 = SimpleDoc.applyIndent indent v1
                cont v2)
            | TableBlock (specs, None, rows) -> 
                workRows rows (fun xs ->
                cont (SimpleDoc.Table(specs, None, xs)))
            | TableBlock (specs, Some headers, rows) -> 
                workRow headers (fun hs ->
                workRows rows (fun xs ->
                cont (SimpleDoc.Table(specs, Some hs, xs))))
        and workRows xs cont = 
            match xs with
            | [] -> cont []
            | d1 :: rest -> 
                workRow d1 (fun v1 ->
                workRows rest (fun vs -> 
                cont (v1::vs)))
        and workRow xs cont = 
            match xs with
            | [] -> cont []
            | cell :: rest -> 
                work cell (fun v1 -> 
                workRow rest (fun vs -> 
                cont (v1::vs)))
        work source (fun x -> x)