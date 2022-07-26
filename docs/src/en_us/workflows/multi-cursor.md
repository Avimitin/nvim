# How to use multi-cursor

I don't enable all the functionality in vim-visual-multi plugins.
Features that I kept are all inspired by
[helix-editor](https://helix-editor.com/) and [Kakoune](https://kakoune.org/):
multiple select and manipulation by words and regex.

## Select multiple lines in same column.

Use <kbd>Ctrl up/down</kbd> to create multiple cursor vertically.

## Select same word.

Use <kbd>\ n</kbd> to select word under cursor, then press <kbd>n</kbd> to select more.

You can also use <kbd>uA</kbd> to select all the same words at once.

## Use regex to select

Use <kbd>ux</kbd> to enable regex search. Input the regex match and press enter to confirm.
Then use <kbd>n</kbd> to select forward, use <kbd>N</kbd> to select backward.

The key <kbd>ux</kbd> is also usable in visual mode. You can use <kbd>V</kbd> to select a region
and then press <kbd>ux</kbd> to regex select region. Just like the Kakoune editor style.

## Add visual select region to multiple cursor

You can use `v` to enter visual select, then press <kbd>ua</kbd> to add the visual select
region.

## How to unselect a matches

You can use <kbd>]</kbd> to select forward, and <kbd>[</kbd> to select backward. Then use
<kbd>Q</kbd> to unselect the matches under cursor.

<sub>More documents: <a href="https://github.com/mg979/vim-visual-multi/wiki/Mappings">vim-visual-multi wiki</a></sub>
