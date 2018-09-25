@echo on

REM blockquotes
pandoc blockquotes.md -f markdown -t docx -s -o blockquotes.docx
pandoc blockquotes.md --metadata pagetitle="Blockquotes" -f markdown+pandoc_title_block -t html -s -o blockquotes.html

pandoc markdown_notes.md -f markdown -t docx -s -o markdown_notes.docx
pandoc --metadata pagetitle="Markdown Notes" markdown_notes.md -f markdown -t html -s -o markdown_notes.html