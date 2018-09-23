# Markdown / Pandoc notes

## Command Line

You can suppress Pandoc's title warning:

    [WARNING] This document format requires a nonempty <title> element.

By supplying pagetile as metadata in the command line, e.g:

    pandoc --metadata pagetitle="Markdown Notes" markdown_notes.md -f markdown -t html -s -o markdown_notes.html

## Paragraphs

This line is ended with  
two spaces (hence it has a hard break)

This line is ended with
nothing (so the line length follows accoring to the output).

This line is ended with nothing

but then there is a blank line (Pandoc HTML renders a vertical gap).

## Comments

[comment]: # This is a comment in the source.

Did you see the comment? Should be 'No'.  
Note, comments must be preceeded with an empty line.

Comment syntax is essentially a link. Calling each comment 'comment' generates
duplicate link warnings, but comments need not be called 'comment'.

[comment2]: # A multiline
[also_a_comment]: # comment.

## Design notes

Because we will not support *introspection* there is no harm in Pandoc-output
rendering to text (or probably better - *lines of text*) in a piecemeal way
for tables and indented structure like list elements. However, it might be
better to have a nicer notion of indenting than we currently have.

