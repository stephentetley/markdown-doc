@echo on

REM blockquotes
pandoc blockquotes.md -f markdown -t docx -s -o blockquotes.docx
pandoc blockquotes.md -f markdown -t html -s -o blockquotes.html