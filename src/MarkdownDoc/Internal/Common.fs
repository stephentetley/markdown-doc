// Copyright (c) Stephen Tetley 2018,2019
// License: BSD 3 Clause

namespace MarkdownDoc.Internal

// Explicitly open all Internal modules.

module Common = 
    
    open System
    open System.Text


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


    type private Lines = string list
    type private Words = string list

    /// Precondition: source is a single line with only spaces (no tabs/newlines)
    let breakline1 (width:int) (source:string) : string list = 
        let words = source.Split(' ') |> Array.toList
        let makeLine (words:Words) : string = String.concat " " (List.rev words)
        let consWords (words:Words) (lines:Lines) : Lines = 
            match words with 
            | [] -> lines
            | _ -> makeLine words :: lines

        let rec work (accLines:Lines) (accWords:Words) (pos:int) (ss:Words) =  
            match ss with 
            | [] -> 
                List.rev <| consWords accWords accLines
            | (w::ws) -> 
                if pos + 1 + w.Length > width then
                    // The first word encountered might be too long..
                    match accWords with
                    | [] -> work (w::accLines) [] 1 ws
                    | _ -> work ((makeLine accWords)::accLines) [w] w.Length ws
                else 
                    work accLines (w::accWords) (pos + 1 + w.Length) ws
        work [] [] 0 words

    /// The input string can be multiline - each component line is then broken 
    /// to the supplied width.
    let breaklines (width:int) (source:string) : string list = 
        let xs = toLines source
        List.map (breakline1 width) xs |> List.concat 




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

