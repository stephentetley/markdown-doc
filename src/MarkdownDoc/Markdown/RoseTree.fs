// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace MarkdownDoc.Markdown

// Don't auto-open, not sure yet the appropriate 
// namespace for this module.

module RoseTree = 
    
    open MarkdownDoc

    // Render a RoseTree as a nested lists.


    // The RoseTree is polymorphic only to allow some flexibility
    // in building it.
    type RoseTree<'a> = 
        | Node of node : 'a * children : RoseTree<'a> list
        
        member v.Label
            with get () : 'a = let (Node(label,_)) = v in label
            
        member v.Kids
            with get ()  : RoseTree<'a> list = let (Node(_,kids)) = v in kids


    type MarkdownRoseTree = RoseTree<Markdown>

    let makeNode (label : 'a) 
                 (kids : RoseTree<'a> list) : RoseTree<'a> = 
        Node(label,kids)

    let makeLeaf (label : 'a) : RoseTree<'a> = 
        Node(label,[])

    let mapTree (mapper : 'a -> 'b) (tree : RoseTree<'a>) : RoseTree<'b> = 
        let rec work (Node(a,kids)) (cont : RoseTree<'b> -> RoseTree<'b>) = 
            let b = mapper a
            workList kids (fun xs -> 
            cont (Node(b, xs)))
         and workList (kids : RoseTree<'a> list) 
                      (cont : RoseTree<'b> list -> RoseTree<'b>) = 
            match kids with
            | [] -> cont []
            | x :: xs -> 
                work x (fun v1 -> 
                workList xs ( fun vs -> 
                cont (v1::vs)))
        work tree (fun x -> x)


    let drawForest (trees : MarkdownRoseTree list) : Markdown = 
        let rec work (level : int) (Node(a,kids)) (cont : Markdown-> Markdown) = 
            let ulist items = 
                match level % 2 with
                | 0 -> unorderedListWithPlus items 
                | _ -> unorderedListWithMinus items 
            workList (level+1) kids (fun xs -> 
            cont (a ^!^ ulist xs))
         and workList (level : int) 
                      (kids : MarkdownRoseTree list) 
                      (cont : Markdown list -> Markdown) = 
            match kids with
            | [] -> cont []
            | x :: xs -> 
                work level x (fun v1 -> 
                workList level xs ( fun vs -> 
                cont (v1::vs)))
        workList 0 trees (fun xs -> unorderedList xs)
    
    let drawTree (tree : MarkdownRoseTree) : Markdown = 
        drawForest [tree]

    let drawNumberedForest (trees : MarkdownRoseTree list) : Markdown = 
        let rec work (Node(a,kids)) (cont : Markdown-> Markdown) = 
            workList kids (fun xs -> 
            cont (a ^!^ orderedList xs))
         and workList (kids : MarkdownRoseTree list) 
                      (cont : Markdown list -> Markdown) = 
            match kids with
            | [] -> cont []
            | x :: xs -> 
                work x (fun v1 -> 
                workList xs ( fun vs -> 
                cont (v1::vs)))
        workList trees (fun xs -> orderedList xs)

    let drawNumberedTree (tree : MarkdownRoseTree) : Markdown = 
        drawNumberedForest [tree]