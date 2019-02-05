// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace MarkdownDoc.Internal

// Explicitly open all Internal modules.

module Syntax = 

    open System.Text
    open MarkdownDoc.Internal.Common

    // ************************************************************************
    // Syntax


    
    type Alignment = 
        | AlignDefault 
        | AlignLeft 
        | AlignCenter 
        | AlignRight

    type ColumnSpec = 
        { Width: int
          Alignment: Alignment }
    
        /// Cell is two characters wider than the specification to allow for left
        /// and right spacing.
        member x.LineSectionWithAlignment 
            with get () : string = 
                match x.Alignment with
                | AlignDefault -> String.replicate (x.Width + 2) "="
                | AlignLeft -> ":" + String.replicate (x.Width + 1) "="
                | AlignCenter -> ":" + String.replicate x.Width "=" + ":"
                | AlignRight -> String.replicate (x.Width + 1) "=" + ":"

        /// Cell is two characters wider than the specification to allow for left
        /// and right spacing.
        member x.LineSection 
            with get () : string = String.replicate (x.Width + 2) "-"


    /// Note - rendering VCatText writes an explicit Markdown 
    /// line break (two trailing spaces) to the output and 
    /// then a new line
    type MdText =
        | EmptyText
        | Text of string
        | HCatText of MdText * MdText
        | VCatText of MdText * MdText

    type MdPara = 
        | EmptyPara
        | ParaText of MdText 
        | UnorderedList of MdPara list
        | OrderedList of MdPara list
        | VCatPara of MdPara * MdPara 

    /// We cache width with cell contents so it can be easily accessed
    type TableCell = 
        { Width: int
          Content: MdPara }

    type TableRow = TableCell list
    
    type MdDoc = 
        | EmptyDoc
        | Paragraph of MdPara
        | BoundedParagraph of int * MdPara
        | Table of ColumnSpec list * TableRow option * TableRow list // bool is has-titles?
        | CodeBlock of MdPara
        | VCatDoc of MdDoc * MdDoc




    // ************************************************************************
    // Markdown builders

    let empty : MdText = EmptyText

    let space : MdText = Text " "

    let beside (x:MdText) (y:MdText) : MdText = 
        match x,y with
        | EmptyText, d -> d
        | d, EmptyText -> d
        | d1,d2 -> HCatText(d1,d2)

    let besideSpace (x:MdText) (y:MdText) : MdText = beside x (beside space y)

    let below (x:MdText) (y:MdText) : MdText = 
        match x,y with
        | EmptyText, d -> d
        | d, EmptyText -> d
        | d1,d2 -> VCatText(d1,d2)

    let textlines (lines:MdText list) : MdText = 
        let rec work zs cont = 
            match zs with
            | [] -> cont empty
            | x :: xs ->
                work xs (fun v1 -> 
                cont (below x v1))
        work lines id

    let stringText (source:string) : MdText = 
        match source with
        | "" -> EmptyText
        | _ -> toLines source |> List.map (fun x -> Text(x)) |> textlines


    let concatMdParas (items:MdPara list) : MdPara = 
        let concat2 a b = 
            match a,b with
            | EmptyPara, d2 -> d2
            | d1, EmptyPara -> d1
            | d1, d2 -> VCatPara(d1,d2)
        List.fold concat2 EmptyPara items


    let concatMdDocs (items:MdDoc list) : MdDoc = 
        let concat2 a b = 
            match a,b with
            | EmptyDoc, d2 -> d2
            | d1, EmptyDoc -> d1
            | d1, d2 -> VCatDoc(d1,d2)
        List.fold concat2 EmptyDoc items

    // ************************************************************************
    // Render Preliminary - render tables



    
    /// Pad one space left and one or more spaces right.
    /// The length of the generated string is `2 + original length`.
    /// The string should not be multiline at this point.
    let private padString (width:int, text:string) = 
        " " + text.PadRight(width + 1 , ' ')

    /// Cells have already been rendered to multiline strings.
    let renderRow (cells:(int * string) list) : string = 
        let widths = List.map fst cells
        let multiCells = List.map (snd >> toLines) cells
        let linearRows = raggedTranspose "" multiCells
        let annoLinearRows = linearRows |> List.map (fun row -> raggedMap2 (fun x y -> (x,y)) widths row)
        let renderLine (cells:(int * string) list) : string = 
            List.map padString cells |> encloseConcat "|" 
        List.map renderLine annoLinearRows |> fromLines
            
    let private gridTableDrawLine (specs:ColumnSpec list) : string = 
        specs |> List.map (fun spec -> spec.LineSection) |> encloseConcat "+" 

    let private gridTableDrawLineWithAlignment (specs:ColumnSpec list) : string = 
        specs |> List.map (fun spec -> spec.LineSectionWithAlignment) |> encloseConcat "+" 



    let textGridTable (columnSpecs:ColumnSpec list) 
                      (headerRow : (string list) option)
                      (contentRows: (string list) list) : string =
        let sb = new StringBuilder ()
        let draw (text:string) : unit = sb.AppendLine(text) |> ignore
        let underline = gridTableDrawLine columnSpecs
        let underlineWithAligments = gridTableDrawLineWithAlignment columnSpecs

        let annotateRow (row:string list) : (int * string) list = 
            raggedMap2 (fun (spec:ColumnSpec) (b:string) -> (spec.Width,b)) columnSpecs row
        
        let drawRow (cells:string list) : unit = 
            let text = annotateRow cells |> renderRow
            draw text; draw underline

        /// Start with a line
        draw underline
        
        /// Draw Header with underline if present
        match headerRow with 
        | None -> ()
        | Some headerCells -> 
            let headerText : string = annotateRow headerCells |> renderRow 
            draw headerText
            draw underlineWithAligments
        
        /// Draw rows and respective underlines
        List.iter drawRow contentRows
        sb.ToString()


    // ************************************************************************
    // Render

    let inline prefixLine (prefix:string) (lineText:string) : string = 
        prefix + lineText

    let padListItem (pad1:string, pad2:string) (body:string) : string = 
        let work xs = 
            match xs with
            | [] -> []
            | x :: xs -> (prefixLine pad1 x) :: List.map (prefixLine pad2) xs
        toLines body |> work |> fromLines


    let unorderedListItem (body:string) : string = 
        padListItem ("*  ", "   ") body

    let orderedListItem (n:int) (body:string) : string = 
        let prefix1 = sprintf "%d.  " n
        let prefix2 = String.replicate (String.length prefix1) " "
        padListItem (prefix1,prefix2) body


    /// Note an item may be a multiline string
    let unordered (items:string list) : string = 
        List.map unorderedListItem items |> fromLines

    /// Note an item may be a multiline string
    let ordered (items:string list) : string = 
        let (xs,_) = List.mapFold (fun n item -> (orderedListItem n item, n+1)) 1 items
        xs |> fromLines

    let renderMdText (text:MdText) : string = 
        let rec work (acc:StringBuilder) (doc:MdText) (cont : StringBuilder -> string) = 
            match doc with
            | EmptyText -> cont acc
            | Text str -> 
                cont (acc.Append(str))
            | HCatText(d1,d2) -> 
                work acc d1 (fun acc1 ->
                work acc1 d2 cont)
            | VCatText(d1,d2) -> 
                work acc d1 (fun acc1 ->
                work (acc1.AppendLine("  ")) d2 cont)
        let sb = new StringBuilder ()
        work sb text (fun x -> x.ToString()) 


    let renderMdPara (para:MdPara) : string =  
        let rec work (acc:StringBuilder) (doc:MdPara) (cont:StringBuilder -> string) = 
            match doc with
            | EmptyPara -> cont acc
            | ParaText txt -> 
                let str = renderMdText txt in cont (acc.Append(str)) 
            | UnorderedList xs ->
                workList [] xs (fun str ->
                cont (acc.Append(unordered str)))
            | OrderedList xs ->
                workList [] xs (fun str ->
                cont (acc.Append(ordered str)))
            | VCatPara(d1,d2) -> 
                work acc d1 (fun acc1 ->
                work (acc1.AppendLine()) d2 cont)
        and workList (acc:string list) (docs:MdPara list) (cont: string list -> string) = 
            match docs with
            | [] -> cont (List.rev acc)
            | d :: ds -> 
                work (new StringBuilder()) d (fun sb1 -> 
                let str1 = sb1.ToString()
                workList (str1::acc) ds cont)
        let sb = new StringBuilder () 
        work sb para (fun x -> x.ToString()) 

    let renderBoundedMdPara (lineWidth:int) (para:MdPara) : string =  
        renderMdPara para |> breaklines lineWidth |> fromLines

    /// Note an item may be a multiline string
    let codeBlock (body:string) : string = 
        toLines body |> List.map (prefixLine "    ") |> fromLines

    /// This renders a row to it's component cell strings.
    /// It is not the final output of a row which has to be transposed into lines.
    let renderTableRow1 (row:TableRow) : string list = 
        let rec workCells (acc:string list) (cells:TableCell list) (cont:string list -> string list) = 
            match cells with
            | [] -> cont (List.rev acc)
            | c1 :: cs -> 
                let str1 = renderBoundedMdPara c1.Width c1.Content
                workCells (str1::acc) cs cont
        workCells [] row id
        

    let renderMdDoc (document:MdDoc) : string = 
        let rec work (acc:StringBuilder) (doc:MdDoc) (cont:StringBuilder -> string) = 
            match doc with
            | EmptyDoc -> cont acc
            | Paragraph para -> 
                let str = renderMdPara para
                cont (acc.AppendLine(str))
            | BoundedParagraph (width,para) -> 
                let str = renderBoundedMdPara width para
                cont (acc.AppendLine(str))
            | CodeBlock para ->
                let str = renderMdPara para |> codeBlock
                cont (acc.AppendLine(str))
            | VCatDoc(d1,d2) -> 
                work acc d1 (fun acc1 -> 
                work (acc1.AppendLine()) d2 cont)
            | Table(columnSpecs,header,rows) -> 
                let headerRow = Option.map renderTableRow1 header
                workRows columnSpecs headerRow [] rows  (fun acc1 -> 
                let tableText = acc1.ToString()
                cont (acc.AppendLine(tableText)))
        and workRows (columnSpecs:ColumnSpec list) (header:(string list) option) 
                     (acc:(string list) list) (rows:TableRow list)  (cont:StringBuilder -> string) = 
            match rows with
            | [] -> let tableText = textGridTable columnSpecs header (List.rev acc)
                    cont (new StringBuilder(value=tableText))
            | x :: xs -> 
                let rowCells = renderTableRow1 x 
                workRows columnSpecs header (rowCells::acc) xs cont

        let sb = new StringBuilder () 
        work sb document (fun x -> x.ToString()) 