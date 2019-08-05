// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace MarkdownDoc.Internal


module GridTable = 


    open MarkdownDoc.Internal.Common

    type Alignment = 
        | AlignDefault 
        | AlignLeft 
        | AlignCenter 
        | AlignRight


    type ColumnSpec = 
        { Width: int
          Alignment: Alignment }

    
    
    /// Pad one space left and one or more spaces right.
    /// The length of the generated string is `2 + original length`.
    /// The string should not be multiline at this point.
    let private padString (width:int, text:string) = 
        " " + text.PadRight(width + 1 , ' ')

    /// Cell is two characters wider than the specification to allow for left
    /// and right spacing.
    let headerSeparator1  (spec : ColumnSpec) : string = 
        match spec.Alignment with
        | AlignDefault -> String.replicate (spec.Width + 2) "="
        | AlignLeft -> ":" + String.replicate (spec.Width + 1) "="
        | AlignCenter -> ":" + String.replicate spec.Width "=" + ":"
        | AlignRight -> String.replicate (spec.Width + 1) "=" + ":"

    let headerSeparator (specs : ColumnSpec list) : string = 
        specs |> List.map headerSeparator1 |> encloseConcat "+" 

    /// Cell is two characters wider than the specification to allow for left
    /// and right spacing.
    let rowSeparator1  (spec : ColumnSpec) : string = 
        String.replicate (spec.Width + 2) "-"

    let rowSeparator (specs : ColumnSpec list) : string = 
        specs |> List.map rowSeparator1 |> encloseConcat "+" 

    /// Cells have already been rendered to multiline strings.
    let renderRow (cells:(int * string) list) : HString = 
        let widths = List.map fst cells
        let multiCells = List.map (snd >> toLines) cells
        let linearRows = raggedTransposeRow multiCells
        let annoLinearRows = linearRows |> List.map (fun row -> raggedMap2 (fun x y -> (x,y)) widths row)
        let renderLine (cells:(int * string) list) : string = 
            List.map padString cells |> encloseConcat "|" 
        List.map renderLine annoLinearRows |> fromListH
            

    /// The content has already been 
    let drawGridTable (columnSpecs : ColumnSpec list) 
                      (headerRow : (string list) option)
                      (contentRows: (string list) list) : HString =
        

        let underline = rowSeparator columnSpecs |> singletonH
        let underlineWithAligments = headerSeparator columnSpecs |> singletonH

        let annotateRow (row:string list) : (int * string) list = 
            raggedMap2 (fun (spec:ColumnSpec) (b:string) -> (spec.Width,b)) columnSpecs row
        
        let drawRow (cells:string list) : HString = 
            let text = annotateRow cells |> renderRow
            appendH text underline

        let drawHeader () = 
            match headerRow with 
            | None -> emptyH
            | Some headerCells -> 
                let headerText : HString = annotateRow headerCells |> renderRow 
                appendH headerText underlineWithAligments

        /// Start with a line
        appendH underline (appendH (drawHeader ()) (concatH <| List.map drawRow contentRows))


    