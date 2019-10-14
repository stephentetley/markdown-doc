// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace MarkdownDoc.Markdown


module Utils = 
    
    open System.Text
    open System.Security.Cryptography

    /// Use this to generate ids for links if we want
    /// reasonable length strings and don't care that 
    /// the the id does not provide semantic information.
    /// Note - for short strings this may generate a longer 
    /// string.
    let md5Hash (source : string) : string = 
        use (md5 : MD5) = MD5.Create()
        let codeArray : byte [] = md5.ComputeHash(Encoding.UTF8.GetBytes(source))
        
        Array.fold (fun (sb:StringBuilder) (a:byte) -> sb.AppendFormat(a.ToString("X2"))) (new StringBuilder ()) codeArray 
            |> fun sb -> sb.ToString()