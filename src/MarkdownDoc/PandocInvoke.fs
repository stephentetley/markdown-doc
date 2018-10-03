// Copyright (c) Stephen Tetley 2018
// License: BSD 3 Clause

namespace MarkdownDoc.Pandoc


[<AutoOpen>]
module Pandoc = 

    let private concatOptions (strs:string list) = 
        String.concat " " <| List.filter (fun ss -> ss<>"") strs

    type Extensions = 
        { EnabledExtensions: string list 
          DisabledExtensions: string list }

    let private renderExtensions (exts:Extensions) : string = 
        let positives = List.map (fun s -> "+" + s) exts.EnabledExtensions
        let negatives = List.map (fun s -> "-" + s) exts.DisabledExtensions
        String.concat "" <| positives @ negatives

    let extensions (exts:string list) : Extensions = 
        { EnabledExtensions = exts 
          DisabledExtensions = [] }

    let enableExtension (ext1:string) (exts:Extensions) = 
        { exts with EnabledExtensions = ext1 :: exts.EnabledExtensions }

    let disableExtension (ext1:string) (exts:Extensions) = 
        { exts with DisabledExtensions = ext1 :: exts.DisabledExtensions }

    type DocxOptions = 
        { ReferenceDoc: option<string>
          DocxExtensions: Extensions }

    let private docxCommand (mdInputPath:string) (outputDocxName:string) (options:DocxOptions) = 
        let referenceDoc = 
            match options.ReferenceDoc with
            | None -> ""
            | Some doc -> sprintf "--reference-doc=%s" doc
        let parts = 
            [ referenceDoc
            ; sprintf "\"%s\"" mdInputPath
            ; "-f markdown"
            ; sprintf "-t docx%s" (renderExtensions options.DocxExtensions) 
            ; "-s"
            ; sprintf "-o \"%s\"" outputDocxName
            ]
        concatOptions parts

    let runPandocDocx (workingDirectory:string) (mdInputPath:string) (opts:DocxOptions) (outputDocxName:string) : unit =
        let command = docxCommand mdInputPath outputDocxName opts
        MarkdownDoc.Internal.Common.shellRun workingDirectory "pandoc" command
