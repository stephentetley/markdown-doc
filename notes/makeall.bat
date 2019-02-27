@echo on

REM blockquotes
pandoc --from=markdown+pandoc_title_block --to=html --metadata pagetitle="Blockquotes"  --standalone --output=output\blockquotes.html blockquotes.md
pandoc --from=markdown --to=docx --reference-doc=include\custom-reference1.docx --standalone --output=output\blockquotes.docx blockquotes.md

REM notes
pandoc --from=markdown --to=html --metadata pagetitle="Markdown Notes" --standalone --output=output\markdown_notes.html markdown_notes.md
pandoc --from=markdown --to=docx --reference-doc=include\custom-reference1.docx --standalone --output=output\markdown_notes.docx markdown_notes.md

REM links (via latex)
pandoc --from=markdown --to=html --metadata pagetitle="Markdown Links" --standalone --output=output\links.html links.md
pandoc --from=markdown --to=latex --variable=linkcolor:"red" --standalone --output=output\links.pdf links.md

REM typographical swaps (via latex)
pandoc --from=markdown --to=html --metadata pagetitle="Markdown Typo" --standalone --output=output\typographic.html typographic.md
pandoc --from=markdown --to=latex --standalone --output=output\typographic.pdf typographic.md
pandoc --from=markdown --to=docx --reference-doc=include\custom-reference1.docx --standalone --output=output\typographic.docx typographic.md

REM operationally
pandoc --metadata pagetitle="Markdown Operationally" --from=markdown --to=html --standalone --output=output\markdown_operationally.html markdown_operationally.md
pandoc --from=markdown --to=latex --standalone --output=output\markdown_operationally.pdf markdown_operationally.md
pandoc --from=markdown --to=docx --reference-doc=include\custom-reference1.docx --standalone --output=output\markdown_operationally.docx markdown_operationally.md

REM imagelinks
pandoc --from=markdown --to=html --metadata pagetitle="Image Links" --standalone --output=image_links.html image_links.md
pandoc --from=markdown --to=docx --reference-doc=include/custom-reference1.docx --standalone --output=output/image_links.docx image_links.md
pandoc --from=markdown --to=latex --standalone --output=output/image_links.pdf image_links.md
