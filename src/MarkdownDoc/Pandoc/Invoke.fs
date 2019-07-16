// Copyright (c) Stephen Tetley 2018,2019
// License: BSD 3 Clause

namespace MarkdownDoc.Pandoc



[<AutoOpen>]
module Invoke = 
    
    open SLFormat.CommandOptions


    open MarkdownDoc.Markdown

    let private ( ++ ) (left : string) (right : string) = left + right
    let private ( +^+ ) (left : string) (right : string) = left + " " + right

    let extensions (exts : Extension list) = 
        String.concat "" <| List.map (fun x -> x.ToString()) exts

    let internal runPandoc1 (shellWorkingDirectory : string) 
                            (args : CmdOpt list) : Result<int,string> =
        SimpleInvoke.executeProcess (Some shellWorkingDirectory) "pandoc" args

    let fromArg (format : string) (inputExtensions : Extension list) : CmdOpt = 
        argument "--from" &= format ++ (extensions inputExtensions)

    let toArg (format : string) (inputExtensions : Extension list) : CmdOpt = 
        argument "--to" &= format ++ (extensions inputExtensions)


    let metadata (key : string) (value :string) : CmdOpt = 
        argument "--metadata" &= key &% value


    let standalone : CmdOpt     = argument "--standalone"
    let output : CmdOpt         = argument "--output"


    /// Option "--reference-doc=FILE"
    /// Reference the supplied "*.docx" or "*.odt" file for custom styles
    let referenceDoc (path:string) : CmdOpt = 
        argument "--reference-doc" &= argValue path

    /// Option "--metadata=pagetitle:"TITLE"
    let metadataPagetitle (title:string) : CmdOpt = 
        metadata "pagetitle" title


    type PandocOptions = 
        { Standalone : bool 
          InputExtensions : Extension list
          OutputExtensions : Extension list
          OtherOptions : CmdOpt list
        }

    let pandocDefaults : PandocOptions = 
        { Standalone = true
          InputExtensions = []
          OutputExtensions = []
          OtherOptions = []  }
            
    let runPandoc (showShellCommand : bool) 
                  (shellWorkingDirectory : string)
                  (fromFormat : string) 
                  (toFormat : string) 
                  (inputPath : string)
                  (outputPath : string)
                  (options : PandocOptions) : Result<int, string> = 
        
        let opts = 
            [ fromArg       fromFormat options.InputExtensions
            ; toArg         toFormat   options.OutputExtensions
            ; (if options.Standalone then standalone else noArgument)
            ; output        &= argValue outputPath
            ; literal       <| doubleQuote inputPath
            ]
        if showShellCommand then
            printfn "Working Directory: %s" shellWorkingDirectory
            renderCmdOpts opts |> printfn "> pandoc %s" 
        else ()

        runPandoc1 shellWorkingDirectory opts

        
     /// Generate plain text from a Markdown file
    let runPandocPlain (showShellCommand : bool) 
                       (shellWorkingDirectory : string) 
                       (inputPath : string) 
                       (outputPath : string) 
                       (options : PandocOptions) : Result<int, string> =
        runPandoc showShellCommand shellWorkingDirectory "markdown" "plain" inputPath outputPath options

        

    /// Generate Docx
    let runPandocDocx (showShellCommand : bool) 
                      (shellWorkingDirectory : string) 
                      (inputPath : string) 
                      (outputPath : string) 
                      (stylesDoc : string option)  
                      (options : PandocOptions) : Result<int, string> =
        let options1 = 
            match stylesDoc with
            | None -> options
            | Some path-> 
                let extras = referenceDoc (argValue path) :: options.OtherOptions 
                { options with OtherOptions = extras }
        runPandoc showShellCommand shellWorkingDirectory "markdown" "docx" inputPath outputPath options1


    let runPandocHtml5 (showShellCommand : bool) 
                       (shellWorkingDirectory : string) 
                       (inputPath : string) 
                       (outputPath : string) 
                       (pageTitle : string option)
                       (options : PandocOptions) : Result<int, string> =
        let options1 = 
            match pageTitle with
            | None -> options
            | Some title -> 
                let extras = metadataPagetitle title :: options.OtherOptions 
                { options with OtherOptions = extras }
        runPandoc showShellCommand shellWorkingDirectory "markdown" "html5" inputPath outputPath options1



 