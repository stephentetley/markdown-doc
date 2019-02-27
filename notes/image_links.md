# Image Links

## Inline Style

This is a blue image.
![Blue](include/blue.jpg)

This should be an ![yellow](./include/yellow.jpg) inline image.

Pandoc's HTML backend does not _copy_ image files, so the link references can
easily be broken.

In `MarkdownDoc` terms, inline image links should be `Text` elements.

Do not quote file names even if they
![large red ](./include/large red.jpg "Large Red Rect") have spaces. Because
the optional title will be quoted Pandoc's parser apparently reads anything
before the first double quote as a file path.

## Reference Style

Reference style links (e.g. ![a green rectangle][greenrect]) give us some
shorthand to use the same image twice ![a green rectangle][greenrect]. Saving
a few characters.

Likewise for images with spaces in the file name ![a big red rectangle][redrect].

[greenrect]: ./include/green.jpg
[redrect]: ./include/large red.jpg
