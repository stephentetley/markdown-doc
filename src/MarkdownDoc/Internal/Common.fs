// Copyright (c) Stephen Tetley 2018,2019
// License: BSD 3 Clause

namespace MarkdownDoc.Internal

// Explicitly open all Internal modules.

module Common = 
    
    open System
    open System.Text


    let internal intersperse (sep:'T) (source:'T list) : 'T list = 
        let rec work (acc:'T list) (xs:'T list) (cont:'T list -> 'T list) = 
            match xs with
            | [] -> cont acc
            | x :: xs -> work (x::sep::acc) xs cont
        match source with
        | [] -> []
        | x :: xs -> work [x] xs (List.rev)




    /// Splits on Environment.NewLine
    let toLines (source:string) : string list = 
        source.Split(separator=[| Environment.NewLine |], options=StringSplitOptions.None) |> Array.toList

    /// Joins with Environment.NewLine
    let fromLines (source:string list) : string = 
        String.concat Environment.NewLine source

    
    let encloseConcat (separator:string) (items:string list) : string = 
        let sb = new StringBuilder(separator)
        List.iter (fun (item:string) -> 
                        sb.Append(item) |> ignore
                        sb.Append(separator) |> ignore) items
        sb.ToString()


    /// FSharp's builtin List.map2 (aka zipWith) throws an exception if one list is shorter than the other.
    /// We prefer truncation instead.
    let raggedMap2 (mapping:'a -> 'b -> 'c) (list1:'a list) (list2:'b list) : 'c list = 
        let rec work acc ls1 ls2 cont = 
            match ls1,ls2 with 
            | [], _ -> cont acc
            | _, [] -> cont acc
            | x::xs, y::ys -> 
                work ((mapping x y)::acc) xs ys cont
        work [] list1 list2 (List.rev)

    /// F#'s built-in List.transpose needs perfect input. It cannot handle ragged tables.
    let raggedTranspose (emptyElement:'a) (table:('a list) list) : ('a list) list = 
        let headsOf (table:('a list) list) : 'a list = 
            List.map (fun xs -> match xs with | [] -> emptyElement; | (x::_) -> x) table
        let tailsOf (table:('a list) list) : ('a list) list = 
            List.map (fun xs -> match xs with | (_::ys) -> ys; | _ -> []) table
        let rec work ac rows = 
            if List.forall (fun (x:'a list)  -> List.isEmpty x) rows then 
                List.rev ac
            else 
                let line1 = headsOf rows
                let rest = tailsOf rows
                work (line1::ac) rest
        work [] table

    
    // ************************************************************************
    // Breaking lines when rendering.
    // We want output to be generally nice looking. i.e
    // paragraphs formatted to lines 80 chars or so except 
    // where the line has unbreakable text like image URLs.


    type internal TextualData1 = 
        | TextualString of string
        | TextualImage of string        // i.e. an unbreakable string
        | TextualSpace 
        member v.Content
            with get() : string = 
                match v with
                | TextualString s -> s
                | TextualImage s -> s
                | TextualSpace -> " "
        member v.Length 
            with get() : int = v.Content.Length

    type internal TextualData = TextualData1 list

    type internal Word = TextualData1

    let private textualToWords (source:TextualData) : Word list = 
        let split1 (text:TextualData1) = 
            match text with
                | TextualSpace -> [TextualSpace]
                | TextualImage(_) -> [text]
                | TextualString s -> 
                    s.Split(separator= [|' '|]) 
                        |> Array.toList 
                        |> List.map TextualString 
                        |> intersperse TextualSpace
        source |> List.map split1 |> List.concat


    /// Assumption - textual data was split with on space, thus we can 
    /// use space to join it together
    let internal wordsToString (source:Word list) : string = 
        source |> List.map (fun x -> x.Content) |> String.concat ""


    /// Precondition: source is a single input line with only 
    /// spaces (no tabs/newlines).
    let breakText (width:int) (source:TextualData) : string list = 
        let makeLine (xs:Word list) : string = List.rev xs |> wordsToString
    
        let consWords (words:TextualData1 list) (lines:string list) : string list = 
            match words with 
            | [] -> lines
            | _ -> makeLine words :: lines

        let rec work (accLines:string list) 
                     (accWords:Word list) (pos:int) (inputs:Word list) 
                     (cont:string list -> string list) =  
            match inputs with 
            | [] -> 
                cont (consWords accWords accLines)
            | (w::ws) -> 
                if pos + 1 + w.Length > width then
                    // The first word encountered might be too long..
                    match accWords with
                    | [] -> 
                        let s1 = makeLine [w] 
                        work (s1 ::accLines) [] 1 ws cont
                    | _ -> work ((makeLine accWords)::accLines) [w] w.Length ws cont
                else 
                    work accLines (w::accWords) (pos + 1 + w.Length) ws cont
        work [] [] 0 (textualToWords source) (fun xs -> List.rev xs)




    // ************************************************************************
    // Invoking markdown

    // Running a process (e.g markdown)
    let private executeProcess (workingDirectory:string) (toolPath:string) (command:string) : Choice<string,int> = 
        try
            let procInfo = new System.Diagnostics.ProcessStartInfo ()
            procInfo.WorkingDirectory <- workingDirectory
            procInfo.FileName <- toolPath
            procInfo.Arguments <- command
            procInfo.CreateNoWindow <- true
            let proc = new System.Diagnostics.Process()
            proc.StartInfo <- procInfo
            proc.Start() |> ignore
            proc.WaitForExit () 
            Choice2Of2 <| proc.ExitCode
        with
        | ex -> Choice1Of2 (sprintf "executeProcess: \n%s" ex.Message)

    let shellRun (workingDirectory:string) (toolPath:string) (command:string)  : unit = 
        try
            match executeProcess workingDirectory toolPath command with
            | Choice1Of2(errMsg) -> failwith errMsg
            | Choice2Of2(code) -> 
                if code <> 0 then
                    failwithf "shellRun fail - error code: %i" code
                else ()
        with
        | ex -> 
            let diagnosis = 
                String.concat "\n" <| 
                    [ ex.Message
                    ; sprintf "Working Directory: %s" workingDirectory 
                    ; sprintf "Command Args: %s" command
                    ]
            failwithf "shellRun exception: \n%s" diagnosis

