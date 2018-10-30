// Copyright (c) Stephen Tetley 2018
// License: BSD 3 Clause

namespace MarkdownDoc.Pandoc


[<AutoOpen>]
module PandocInvoke = 

    let private doubleQuote (s:string) : string = "\"" + s + "\""

    let private concatOptions (strs:string list) = 
        String.concat " " <| List.filter (fun ss -> ss<>"") strs

    type Extension =
        | Enable of string
        | Disable of string
        override x.ToString() = 
            match x with
            | Enable s -> "+" + s
            | Disable s -> "-" + s

    type Format = 
        { FormatName: string 
          Extensions: Extension list }

        override x.ToString() = 
            let exts = List.map (fun (x:Extension) -> x.ToString()) x.Extensions
            String.concat "" (x.FormatName :: exts)

    /// KeyValue options are also printed as verbose                
    type Option = 
        | Short of char * string option
        | Verbose of string * string option 
        | KeyValue of string * (string * string) 
        override x.ToString() = 
            let printBody (body:option<string>) =
                match body with
                | None | Some null | Some "" -> ""
                | Some s -> "=" + s
            let printArgs (k:string,v:string) : string = 
                match v with
                | null | "" -> k
                | _ -> k + ":" + v                
            match x with
            | Short(ch,body) -> "-" + ch.ToString() + printBody body
            | Verbose(str,body) -> "--" + str + printBody body
            | KeyValue(str,body) -> sprintf "--%s=%s" str (printArgs body)


    let commandBody (fromFormat:Format) (toFormat:Format) 
                        (inputPath:string) (options:Option list) : string = 
        sprintf "--from=%s --to=%s %s %s" 
                (fromFormat.ToString())
                (toFormat.ToString())
                inputPath
                (String.concat " " <| List.map (fun (o:Option) -> o.ToString()) options)


    type PandocArgs = 
        { FromFormat: Format
          InputPath: string 
          ToFormat: Format
          OutputPath: string 
          Options: Option list }


    let runPandoc (shellWorkingDirectory:string) (args:PandocArgs) : unit =
        let output = Verbose("output", Some <| doubleQuote args.OutputPath)
        let options = args.Options @ [output]
        let command = commandBody args.FromFormat args.ToFormat (doubleQuote args.InputPath) options
        MarkdownDoc.Internal.Common.shellRun shellWorkingDirectory "pandoc" command


    /// --reference-doc
    /// For docx / odt output
    let referenceDoc (path:string) : Option = 
        Verbose("reference-doc", Some path)

    let standalone : Option = Verbose("standalone", None)

    let metadata (key:string) (value:string) : Option = KeyValue("metadata", (key,value))

    let enableTableCaptions : Extension = Enable("table_captions")

    let runPandocDocx (shellWorkingDirectory:string) 
                        (inputPath:string) 
                        (outputPath:string) 
                        (stylesDoc:string) 
                        (otherOptions: Option list) : unit =
        let args = 
            { FromFormat = { FormatName = "markdown"; Extensions = [] }
            ; InputPath = inputPath 
            ; ToFormat = { FormatName = "docx"; Extensions = [enableTableCaptions] }
            ; OutputPath = outputPath
            ; Options = 
                match stylesDoc with
                | null | "" -> (standalone :: otherOptions)
                | _ -> (referenceDoc stylesDoc :: standalone :: otherOptions) }
        runPandoc shellWorkingDirectory args

    let runPandocHtml (shellWorkingDirectory:string) 
                            (inputPath:string) 
                            (outputPath:string) 
                            (pageTitle:string)
                            (otherOptions:Option list) : unit =
        let makePageTile (s:string) = 
            metadata "pagetitle" (doubleQuote s)
        let args = 
            { FromFormat = { FormatName = "markdown"; Extensions = [] }
            ; InputPath = inputPath 
            ; ToFormat = { FormatName = "html"; Extensions = [enableTableCaptions] }
            ; OutputPath = outputPath
            ; Options = 
                match pageTitle with
                | null | "" -> (standalone :: otherOptions)
                | _ -> (makePageTile pageTitle :: standalone :: otherOptions) }
        runPandoc shellWorkingDirectory args
