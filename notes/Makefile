
all: blockquotes notes links typographic operationally

blockquotes:
	pandoc --from=markdown+pandoc_title_block --to=html --metadata pagetitle="Blockquotes"  --standalone --output=output/blockquotes.html blockquotes.md
	pandoc --from=markdown --to=docx --reference-doc=include/custom-reference1.docx --standalone --output=output/blockquotes.docx blockquotes.md

notes:
	pandoc --from=markdown --to=html --metadata pagetitle="Markdown Notes" --standalone --output=output/markdown_notes.html markdown_notes.md
	pandoc --from=markdown --to=docx --standalone --output=output/markdown_notes.docx markdown_notes.md

links:
	pandoc --from=markdown --to=html --metadata pagetitle="Markdown Links" --standalone --output=output/links.html links.md
	pandoc --from=markdown --to=latex --standalone --variable=linkcolor:"red" --output=output/links.pdf links.md

typographic:
	pandoc --from=markdown --to=html --metadata pagetitle="Markdown Typo" --standalone --output=output/typographic.html typographic.md
	pandoc --from=markdown --to=latex --standalone --output=output/typographic.pdf typographic.md
	pandoc --from=markdown --to=docx --reference-doc=include/custom-reference1.docx --standalone --output=output/typographic.docx typographic.md

operationally:
	pandoc --from=markdown+superscript+subscript --to=html --metadata pagetitle="Markdown, Operationally" --standalone --output=output/markdown_operationally.html markdown_operationally.md
	pandoc --from=markdown+superscript+subscript --to=latex --standalone --output=output/markdown_operationally.pdf markdown_operationally.md
	pandoc --from=markdown+superscript+subscript --to=docx --reference-doc=include/custom-reference1.docx --standalone --output=output/markdown_operationally.docx markdown_operationally.md
