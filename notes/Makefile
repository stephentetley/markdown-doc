
all: blockquotes notes links typographic

blockquotes:
	pandoc --from=markdown --to=docx --standalone --output=blockquotes.docx blockquotes.md 
	pandoc --from=markdown+pandoc_title_block --to=html --metadata pagetitle="Blockquotes"  --standalone --output=blockquotes.html blockquotes.md 

notes:
	pandoc --metadata pagetitle="Markdown Notes" -f markdown -t html --standalone --output=markdown_notes.html markdown_notes.md
	pandoc --from=markdown --to=docx --standalone --output=markdown_notes.docx markdown_notes.md

links:
	pandoc --metadata pagetitle="Markdown Links" --from=markdown --to=html --standalone --output=links.html links.md
	pandoc --from=markdown --to=latex --standalone --output=links.pdf --variable=linkcolor:"red" links.md

typographic:
	pandoc --metadata pagetitle="Markdown Typo" --from=markdown --to=html --standalone --output=typographic.html typographic.md
	pandoc --from=markdown --to=latex --standalone --output=typographic.pdf typographic.md
	pandoc --from=markdown --to=docx --reference-doc=custom-reference1.docx --standalone --output=typographic.docx typographic.md

