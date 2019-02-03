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

Currently (3 Feb 2019), `Markdown-doc` has no special designation for lists,
the implementation renders the item-body at _call time_ and stores it as a
list of strings.

Worth noting - in Markdown there isn't a concrete notion of _lists_ like there
there is in HTML; there are just one-or-more list items in sequence.


## Tables

Tables are complicated because unlike other objects built from tiles they have
a horizontal as well vertical aspect.

Like lists, currently (3 Feb 2019) `Markdown-doc` has no special designation
for tables the implementation renders cells in rows, then rows into a table
at _call time_, storing the output as a list of strings.
