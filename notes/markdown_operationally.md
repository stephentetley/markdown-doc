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

## Code Blocks

Code blocks are a Tile transformation:  
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
combinators in general. An infix symbol seems necessary, how about ^**^ ?

    rawtext "e=mc" ^**^ int 2

e=mc^2^

Subscript is tilde.

Some text with text~below~ it.

Note this is Pandoc specific and requires the superscript extension.  
Also attention is needed to escape spaces in the text of the superscript. This
might imply we need a text transformer function that looks inside the text of
the superscript and escapes space characters with backslash `\`.
