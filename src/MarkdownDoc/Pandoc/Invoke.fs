// Copyright (c) Stephen Tetley 2018,2019
// License: BSD 3 Clause

namespace MarkdownDoc.Pandoc



[<AutoOpen>]
module Invoke = 
    
    open SLFormat.CommandOptions


    open MarkdownDoc.Markdown

    

    let internal runPandoc1 (shellWorkingDirectory:string) (args:CmdOpt list) : unit =
        SimpleInvoke.runProcessSimple shellWorkingDirectory "pandoc" args

    let fromLong : CmdOpt       = argument "--from"
    let toLong : CmdOpt         = argument "--to"
    let metadata : CmdOpt       = argument "--metadata"
    let standalone : CmdOpt     = argument "--standalone"
    let output : CmdOpt         = argument "--output"


    /// Option "--reference-doc=FILE"
    /// Reference the supplied "*.docx" or "*.odt" file for custom styles
    let referenceDoc (path:string) : CmdOpt = argument "--reference-doc" &= argValue path

    /// Option "--metadata=pagetitle:"TITLE"
    let metadataPagetitle (title:string) : CmdOpt = metadata &= "pagetitle" &% title


    type PandocOptions = 
        { Standalone: bool 
          InputExtensions: Extension list
          OutputExtensions: Extension list
          OtherOptions: CmdOpt list
        }

    let pandocDefaults :PandocOptions = 
        { Standalone = true
          InputExtensions = []
          OutputExtensions = []
          OtherOptions = []  }
            
    let runPandoc (shellWorkingDirectory:string)
                  (fromFormat:string) 
                  (toFormat:string) 
                  (inputPath:string)
                  (outputPath:string)
                  (options:PandocOptions) : unit = 
        let opts = 
            [ fromLong      &= fromFormat &** options.InputExtensions
            ; toLong        &= toFormat   &** options.OutputExtensions
            ; (if options.Standalone then standalone else noArgument)
            ; output        &= argValue outputPath
            ; literal       <| argValue inputPath
            ]
        runPandoc1 shellWorkingDirectory opts

    let execPandoc (shellWorkingDirectory:string) 
                   (fromFormat:string) 
                   (toFormat:string) 
                   (outputPath:string) 
                   (options:PandocOptions)
                   (doc:Markdown) : unit =
        let mdpath = System.IO.Path.ChangeExtension(outputPath, "md")
        doc.Save(mdpath)
        runPandoc shellWorkingDirectory fromFormat toFormat mdpath outputPath options

        
     /// Generate plain text from a Markdown file
    let runPandocPlain (shellWorkingDirectory:string) 
                       (inputPath:string) 
                       (outputPath:string) 
                       (options:PandocOptions) : unit =
        runPandoc shellWorkingDirectory "markdown" "plain" inputPath outputPath options

     /// Generate plain text from a Markdown Doc.
    let execPandocPlain (shellWorkingDirectory:string) 
                        (outputPath:string) 
                        (options:PandocOptions) 
                        (doc:Markdown): unit =
        execPandoc shellWorkingDirectory "markdown" "plain" outputPath options doc
        

    /// Generate Docx
    let runPandocDocx (shellWorkingDirectory:string) 
                      (inputPath:string) 
                      (outputPath:string) 
                      (stylesDoc:string option)  
                      (options:PandocOptions) : unit =
        let options1 = 
            match stylesDoc with
            | None -> options
            | Some path-> 
                let extras = referenceDoc (argValue path) :: options.OtherOptions 
                { options with OtherOptions = extras }
        runPandoc shellWorkingDirectory "markdown" "docx" inputPath outputPath options1
    
    let execPandocDocx (shellWorkingDirectory:string) 
                       (outputPath:string) 
                       (stylesDoc:string option) 
                       (options:PandocOptions) 
                       (doc:Markdown): unit =
        let mdpath = System.IO.Path.ChangeExtension(outputPath, "md")
        doc.Save(mdpath)
        runPandocDocx shellWorkingDirectory mdpath outputPath stylesDoc options

    let runPandocHtml (shellWorkingDirectory:string) 
                      (inputPath:string) 
                      (outputPath:string) 
                      (pageTitle:string option)
                      (options:PandocOptions) : unit =
        let options1 = 
            match pageTitle with
            | None -> options
            | Some title -> 
                let extras = metadataPagetitle title :: options.OtherOptions 
                { options with OtherOptions = extras }
        runPandoc shellWorkingDirectory "markdown" "html" inputPath outputPath options1

    let execPandocHtml (shellWorkingDirectory:string) 
                       (outputPath:string) 
                       (pageTitle:string option)
                       (options:PandocOptions) 
                       (doc:Markdown): unit =
        let mdpath = System.IO.Path.ChangeExtension(outputPath, "md")
        doc.Save(mdpath)
        runPandocHtml shellWorkingDirectory mdpath outputPath pageTitle options

 