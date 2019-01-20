// Copyright (c) Stephen Tetley 2018,2019
// License: BSD 3 Clause

namespace MarkdownDoc.Pandoc


[<AutoOpen>]
module Invoke = 
    
    open MarkdownDoc.Internal
    open MarkdownDoc.Markdown

    let private doubleQuote (s:string) : string = "\"" + s + "\""

    let private concatOptions (strs:string list) = 
        String.concat " " <| List.filter (fun ss -> ss<>"") strs


    type PandocExtension =
        | Enable of string
        | Disable of string
        override x.ToString() = 
            match x with
            | Enable s -> "+" + s
            | Disable s -> "-" + s

    type PandocFormat = 
        { FormatName: string 
          Extensions: PandocExtension list }

        override x.ToString() = 
            let exts = 
                List.map (fun (x:PandocExtension) -> x.ToString()) x.Extensions
            String.concat "" (x.FormatName :: exts)

    /// KeyValue options are also printed as verbose                
    type PandocOption = 
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


    let commandBody (fromFormat:PandocFormat) (toFormat:PandocFormat) 
                        (inputPath:string) (options:PandocOption list) : string = 
        sprintf "--from=%s --to=%s %s %s" 
                (fromFormat.ToString())
                (toFormat.ToString())
                inputPath
                (String.concat " " <| List.map (fun (o:PandocOption) -> o.ToString()) options)


    type PandocArgs = 
        { FromFormat: PandocFormat
          InputPath: string 
          ToFormat: PandocFormat
          OutputPath: string 
          Options: PandocOption list }


    let runPandoc (shellWorkingDirectory:string) (args:PandocArgs) : unit =
        let output = Verbose("output", Some <| doubleQuote args.OutputPath)
        let options = args.Options @ [output]
        let command = commandBody args.FromFormat args.ToFormat (doubleQuote args.InputPath) options
        shellRun shellWorkingDirectory "pandoc" command


    /// --reference-doc
    /// For docx / odt output
    let referenceDoc (path:string) : PandocOption = 
        Verbose("reference-doc", Some path)

    let standalone : PandocOption = 
        Verbose("standalone", None)

    let metadata (key:string) (value:string) : PandocOption = 
        KeyValue("metadata", (key,value))

    let enableTableCaptions : PandocExtension = 
        Enable("table_captions")


    /// Generate Docx
    let runPandocDocx (shellWorkingDirectory:string) 
                        (inputPath:string) 
                        (outputPath:string) 
                        (stylesDoc:string) 
                        (otherOptions: PandocOption list) : unit =
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
    
    /// Generate Docx
    let pandocGenerateDocx (shellWorkingDirectory:string) 
                            (doc:Markdown) 
                            (outputPath:string) 
                            (stylesDoc:string) 
                            (otherOptions:PandocOption list) : unit =
        let mdpath = System.IO.Path.ChangeExtension(outputPath, "md")
        doc.Save(mdpath)
        runPandocDocx shellWorkingDirectory mdpath outputPath stylesDoc otherOptions


    /// Generate HTML
    let runPandocHtml (shellWorkingDirectory:string) 
                            (inputPath:string) 
                            (outputPath:string) 
                            (pageTitle:string)
                            (otherOptions:PandocOption list) : unit =
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

    /// Generate HTML
    let pandocGenerateHtml (shellWorkingDirectory:string) 
                            (doc:Markdown) 
                            (outputPath:string) 
                            (pageTitle:string)
                            (otherOptions:PandocOption list) : unit =
        let mdpath = System.IO.Path.ChangeExtension(outputPath, "md")
        doc.Save(mdpath)
        runPandocHtml shellWorkingDirectory mdpath outputPath pageTitle otherOptions

    /// Generate Plain text
    let runPandocPlain (shellWorkingDirectory:string) 
                            (inputPath:string) 
                            (outputPath:string) 
                            (otherOptions:PandocOption list) : unit =
        let args = 
            { FromFormat = { FormatName = "markdown"; Extensions = [] }
            ; InputPath = inputPath 
            ; ToFormat = { FormatName = "plain"; Extensions = [] }
            ; OutputPath = outputPath
            ; Options = (standalone :: otherOptions) }
        runPandoc shellWorkingDirectory args

    /// Generate Plain text
    let pandocGeneratePlain (shellWorkingDirectory:string) 
                            (doc:Markdown) 
                            (outputPath:string) 
                            (otherOptions:PandocOption list) : unit =
        let mdpath = System.IO.Path.ChangeExtension(outputPath, "md")
        doc.Save(mdpath)
        runPandocPlain shellWorkingDirectory mdpath outputPath otherOptions
