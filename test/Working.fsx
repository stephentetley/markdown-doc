// Copyright (c) Stephen Tetley 2018,2019
// License: BSD 3 Clause

#r "netstandard"
open System.Text

#I @"C:\Users\stephen\.nuget\packages\slformat\1.0.2-alpha-20190313\lib\netstandard2.0"
#r "SLFormat.dll"

#load "..\src\MarkdownDoc\Internal\Common.fs"
#load "..\src\MarkdownDoc\Internal\Syntax.fs"
#load "..\src\MarkdownDoc\Markdown.fs"

open MarkdownDoc

let sample = text "Hello" ^^ character ' '  ^^ text "World!"

let demo01 () =
    testRenderText sample

let demo02 () =
    let olist = orderedList [paraText sample; paraText sample; paraText sample]
    let ulist = unorderedList [olist; paraText sample; paraText sample]
    testRender (markdown ulist)


let demo03 () =

    let header = fixedWidthMarkdown 80 (paraText (rawtext "# Title"))
    let olist = orderedList [paraText sample; paraText sample; paraText sample]
    let ulist = unorderedList [olist; paraText sample; paraText sample]
    let para1 = fixedWidthMarkdown 80 ulist
    let document = header ^@^ para1
    testRender document

let test01 () = 
    let text = "one two  three   four"
    let splits = text.Split(' ')
    String.concat " " splits 

let test02 () = 
    Internal.Common.raggedMap2 (sprintf "%c-%d") ['a';'b';'c';'d'] [1;2;3;4;5] 

type TextualData1 = 
    | TextString of string
    | TextImage of string        // i.e. an unbreakable string
    member v.Content
        with get() : string = 
            match v with
            | TextString s -> s
            | TextImage s -> s

    member v.Length 
        with get() : int = v.Content.Length


type TextualData = TextualData1 list

type Word = TextualData1

let textualToWords (source:TextualData) : Word list = 
    let split1 (text:TextualData1) = 
        match text with
            | TextImage(_) -> [text]
            | TextString s -> 
                s.Split(separator= [|' '|]) |> Array.toList |> List.map TextString 
    source |> List.map split1 |> List.concat


/// Assumption - textual data was split with on space, thus we can 
/// use space to join it together
let wordsToString (source:Word list) : string = 
    source |> List.map (fun x -> x.Content) |> String.concat " "


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

let sample1 : TextualData = 
    [ TextString "This is an"
    ; TextImage "<img='parent/file1.jpg'>"
    ; TextString "inline image."
    ]

let demo04 () = 
    textualToWords sample1

let demo05 () = 
    breakText 12 sample1


let demo06 () = 
    breakText 20 sample1