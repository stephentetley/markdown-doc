@echo on

REM blockquotes
pandoc blockquotes.md -f markdown -t docx -s -o blockquotes.docx
pandoc blockquotes.md --metadata pagetitle="Blockquotes" -f markdown+pandoc_title_block -t html -s -o blockquotes.html

REM notes
pandoc markdown_notes.md -f markdown -t docx -s -o markdown_notes.docx
pandoc --metadata pagetitle="Markdown Notes" markdown_notes.md -f markdown -t html -s -o markdown_notes.html


REM links (via latex)
pandoc --metadata pagetitle="Markdown Links" --from=markdown --to=html --standalone --output=links.html links.md
pandoc --from=markdown --to=latex --standalone --output=links.pdf --variable=linkcolor:"red" links.md