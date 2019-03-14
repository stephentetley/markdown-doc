// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace MarkdownDoc.Internal

// Explicitly open all Internal modules.

[<RequireQualifiedAccess>]
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
    /// RawText is text that will not be browen over consecutive
    /// lines when rendered (e.g. paths in links).
    /// Group changes the render context to render on a single 
    /// unbroken line.
    type MdText =
        private | EmptyText
                | Text of string
                | HCatText of MdText * MdText
                | VCatText of MdText * MdText
                | Group of MdText
        static member empty : MdText = EmptyText

    type MdPElement = 
        | EmptyPE
        | ParaText of MdText 
        | UnorderedList of MdPElement list
        | OrderedList of MdPElement list
        | VCatPara of MdPElement * MdPElement 
        static member empty : MdPElement = EmptyPE

    /// We cache width with cell contents so it can be easily accessed
    type TableCell = 
        { Width: int
          Content: MdPElement }

    type TableRow = TableCell list
    
    type MdDoc = 
        | EmptyDoc
        | Paragraph of int * MdPElement
        | Table of ColumnSpec list * TableRow option * TableRow list // bool is has-titles?
        | CodeBlock of MdPElement
        | VCatDoc of MdDoc * MdDoc
        static member empty : MdDoc = EmptyDoc



    // ************************************************************************
    // Markdown builders

    let empty : MdText = EmptyText

    let inline groupText (text:MdText) = Group text

    /// No escaping, or line splitting.
    let inline rawText (contents:string) = Text contents

    let inline escapedText (content:string) : MdText = 
        /// Ampersand must be replaced first, otherwise we get double escaping.
        let s1 = content.Replace("&", "&amp;").Replace("<", "&lt;")
        Text s1  

    let space : MdText = Text " "

    let beside (x:MdText) (y:MdText) : MdText = 
        match x,y with
        | EmptyText, d -> d
        | d, EmptyText -> d
        | d1,d2 -> HCatText(d1,d2)

    let besideSpace (x:MdText) (y:MdText) : MdText = beside x (beside space y)

    /// Vertically concatenate two lines of text.
    let belowText (x:MdText) (y:MdText) : MdText = 
        match x,y with
        | EmptyText, d -> d
        | d, EmptyText -> d
        | d1,d2 -> VCatText(d1,d2)


    /// Vertically concat the list lines of text.
    let belowTexts (lines:MdText list) : MdText = 
        let rec work zs cont = 
            match zs with
            | [] -> cont empty
            | x :: xs ->
                work xs (fun v1 -> 
                cont (belowText x v1))
        work lines id


    let stringText (source:string) : MdText = 
        match source with
        | "" -> EmptyText
        | _ -> toLines source |> List.map (fun x -> Text(x)) |> belowTexts

    let belowPElement (d1:MdPElement) (d2:MdPElement) : MdPElement = 
        match d1,d2 with
        | EmptyPE, b -> b
        | a, EmptyPE -> a
        | a, b -> VCatPara(a,b)

    let concatMdPElements (items:MdPElement list) : MdPElement = 
        List.fold belowPElement EmptyPE items


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

    type internal SimpleDoc = TextualData1
    type internal SimpleLine = SimpleDoc list

    let textToSimpleLines (text:MdText) : SimpleLine list = 
        let consWords (words:SimpleDoc list) 
                      (lines:SimpleLine list) : SimpleLine list = 
            (List.rev words) :: lines
        let groupWordsIntoImage (words:SimpleDoc list) 
                                (lines:SimpleLine list) : SimpleDoc = 
             consWords words lines
                |> List.rev
                |> List.map wordsToString
                |> String.concat " "
                |> TextualImage 
        let rec work (accLines:SimpleLine list) 
                     (accWords:SimpleDoc list)
                     (input:MdText) 
                     (cont : SimpleLine list -> SimpleDoc list -> SimpleLine list) = 
            match input with
            | EmptyText -> cont accLines accWords
            | Text str -> 
                cont accLines (TextualString str :: accWords)
            | HCatText(d1,d2) -> 
                work accLines accWords d1 (fun ls1 ws1 ->
                work ls1 ws1 d2 cont)
            | VCatText(d1,d2) -> 
                work accLines accWords d1 (fun ls1 ws1 ->
                work (consWords ws1 ls1) [] d2 cont)  
            | Group (d1) -> 
                /// Empty the accumulators
                work [] [] d1 (fun ls ws -> 
                let image1 = groupWordsIntoImage ws ls
                cont accLines (image1::accWords))
                    
        work [] [] text (fun lines words -> consWords words lines |> List.rev) 

    let renderSimpleLines (width:int) (lines: SimpleLine list) : string = 
        lines |> List.map (breakText width)
              |> List.map fromLines
              |> fromLines


    let renderMdText (width:int) (text:MdText) : string = 
        text |> textToSimpleLines
             |> renderSimpleLines width 



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

    /// TODO - This needs a close read over and testing.
    let renderMdPElement (lineWidth:int) (para:MdPElement) : string =  
        let rec work (acc:StringBuilder) 
                     (doc:MdPElement) (cont:StringBuilder -> string) = 
            match doc with
            | EmptyPE -> cont acc
            | ParaText txt -> 
                let str = renderMdText lineWidth txt
                cont (acc.Append(str)) 
            | UnorderedList xs ->
                workList [] xs (fun strs ->
                cont (acc.Append(unordered strs)))
            | OrderedList xs ->
                workList [] xs (fun strs ->
                cont (acc.Append(ordered strs)))
            | VCatPara(d1,d2) -> 
                work acc d1 (fun acc1 ->
                work (acc1.AppendLine()) d2 cont)
        and workList (acc:string list) 
                     (docs:MdPElement list) (cont: string list -> string) = 
            match docs with
            | [] -> cont (List.rev acc)
            | d :: ds -> 
                work (new StringBuilder()) d (fun sb1 -> 
                let str1 = sb1.ToString()
                workList (str1::acc) ds cont)
        let sb = new StringBuilder () 
        work sb para (fun x -> x.ToString()) 

    let renderBoundedMdPara (lineWidth:int) (para:MdPElement) : string =  
        renderMdPElement lineWidth para 

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
            | Paragraph (width,para) -> 
                let str = renderMdPElement width para
                cont (acc.AppendLine(str))
            | CodeBlock para ->
                let str = renderMdPElement 800 para |> codeBlock
                cont (acc.AppendLine(str))
            | VCatDoc(d1,d2) -> 
                work acc d1 (fun acc1 -> 
                work (acc1.AppendLine()) d2 cont)
            | Table(columnSpecs,header,rows) -> 
                /// Send a partially instantiated table-text building function to `WorkRows`
                let tableToString rows =
                     textGridTable columnSpecs (Option.map renderTableRow1 header) rows
                workRows tableToString [] rows (fun acc1 -> 
                let tableText = acc1.ToString()
                cont (acc.AppendLine(tableText)))
        and workRows (makeTableText:(string list) list -> string)
                     (acc:(string list) list) (rows:TableRow list) 
                       (cont:StringBuilder -> string) = 
            match rows with
            | [] -> let tableText = makeTableText (List.rev acc)
                    cont (new StringBuilder(value=tableText))
            | x :: xs -> 
                let rowCells = renderTableRow1 x 
                workRows makeTableText (rowCells::acc) xs cont

        let sb = new StringBuilder () 
        work sb document (fun x -> x.ToString()) 