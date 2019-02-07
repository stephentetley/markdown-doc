# Markdown, operationally...

Terminology note - **tile** is basically a paragraph / text block. **Text** is
the contents of a tile (words, numbers spaces, and intra-text markup like
**emphasis** or `inline code`).

## Tile concatenation

Tiles always concatenate vertically. There should be a blank line separating
each tile.

## Inline Code

Intra-text markup for inline code such as `printfn "hello World"` etc.

Also prove a double backticked variant, and delegate to the user whether the
believe the quoted code contains backticks or not.

## Emphasis

**Bold** and *italic* are simple text transformers (the implementation is
simply *nesting*).

## Code Blocks

Code blocks are a tile transformation:  
Prefix each line with 4 spaces.


## Manual Line Breaks

End the line with two spaces.  
This seems like a concat operator for Text:

    line1 <@\> line2

Plus a list combinator to for multiple lines:

    hardLines [line1; line2; ...]

## Page Breaks

A _tile builder_ that creates a tile of three asterisks: `***`.  
Tile concatenation means that it a page break will be printed in on a separate
line.

## Strikethrough

A text transformer.

    strikethrough(rawtext "Hello" ^+^ rawtext "world!")

Gives ~~Hello world!~~.   

## Superscript and Subscript

Superscript uses the `^` operator in Markdown that we are fond of for naming
combinators in general. An infix symbol is a possibility, how about ^**^ ?

    rawtext "e=mc" ^**^ int 2

e=mc^2^

However, this has the problem that we might have to bracket code for
precedence. Maybe using the regular concat operators is better and implement
subscript as a text transformer:

    rawtext "e=mc" <> superscript (int 2)

This code is very clear about what is superscripted.

Subscript is tilde.

Some text with text~below~ it.

Note this is Pandoc specific and requires the superscript extension.  
Also attention is needed to escape spaces in the text of the superscript. This
might imply we need a text transformer function that looks inside the text of
the superscript and escapes space characters with backslash `\`.

## Lists

* A list item should be a tile.  
  It can contain multiline  
  text.

  Or even breaks, implying concatenation of two or more tiles.
* Lists can also have:
  * Sub lists
  * Like this.

When these notes were started (3 February), `Markdown-doc` had no special
designation for lists, the implementation rendered the item-body at
_call time_ and stored it as a list of strings.

Worth noting - in Markdown there isn't a concrete notion of _lists_ like there
there is in HTML; there are just one-or-more _list items_ in sequence.


## Tables

Tables are complicated because unlike other objects built from tiles they have
a horizontal as well vertical aspect.

Like lists, circa the start of February, `Markdown-doc` had no special
designation for tables the implementation rendered the cells in a rows, then
the rows into a table at _call time_, storing the output as a list of strings.

## Digression #1

At the start of February, this was the first attempt at a structured syntax:

    type TextWidth = int

    type MdTile =
        | Empty
        | Unbound of MarkdownText.MdText
        | Bound of TextWidth * MarkdownText.MdText
        | VCat of MdTile * MdTile
        | CodeBlock of MdTile
        | OrderedList of MdTile list
        | UnorderedList of MdTile list
        | Table of TableRow option * TableRow list
    and TableCell =
        | TableCell of TextWidth * MdTile
    and TableRow =
        | TableRow of TableCell list

It has at least one _error_ - the renderer should be print a sequence of tiles
with a separating blank line but in hand written markup it's common for a list
to be appended to a paragraph. In syntax terms, ordered and unordered lists
are a _smaller_ entity than say a table or a header.

There are also problems that make the syntax unsatisfactory. There is too much
recursion - tables can contain lists (good) but lists can contain tables (bad)
and tables can contain tables (bad). There is no syntax for headers so anything
could contain headers. Having a lot of recursion in the syntax makes the
rendering code very tangled.

These problems imply that there should be a _medium-size_ syntax element
between tiles and text that can hold lists and paragraph text but not tables
or headers.
