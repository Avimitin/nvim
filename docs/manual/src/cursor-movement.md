# Cursor Movement

If you are using vim for the first time, you might want to use arrow key to navigate.
But actually vim cursor movement is based on four key:

```text
   ^
   k
<h   l>
   j
   v
```

You will need sometime to get used to this navigate mechanism. However once you get used
to it, you will find it really handy because you don't need to twist your wrist.

To move faster, you can combine `hjkl` with key `<shift>`. `<shift>j`, the capitalized `J`,
can move down your cursor by 5 lines. `<shift>k`, can move up your cursor by 5 lines. And the
`H` key is like `<Home>` key, it will navigate to the first character of the current line. `L`
is the opposite, it will navigate to the last character of the current line.

Belows are cursor movement cheatsheets in Normal mode.

| key             | action                                         |
| --------------- | ---------------                                |
| j               | Move down one line                             |
| k               | Move up one line                               |
| h               | Go one character left                          |
| l               | Go one character right                         |
| J               | Move down 5 lines                              |
| K               | Move up 5 lines                                |
| H               | Go to the first character of the current line  |
| L               | Go to the last character of the current line   |
| w               | Jump to next word                              |
| b               | Jump back one word                             |
| W               | Jump to 5 next words                           |
| B               | Jump back 5 words                              |
| e               | Jump to the last character of the current word |
| gg              | Jump to the first line of the current file     |
| G               | Jump to the last line of the current file      |

Belows are cursor movement in Insert mode:

| Key        | Action                              |
|------------|-------------------------------------|
| `<Ctrl-e>` | Go to the end of the current line   |
| `<Ctrl-a>` | Go to the front of the current line |
| `<Ctrl-f>` | Forward one word                    |
| `<Ctrl-b>` | Backward one word                   |
| `<Ctrl-n>` | Go down one line                    |
| `<Ctrl-p>` | Go up one line                      |

## Search based cursor movement

The plugin [`leap.nvim`](https://github.com/ggandor/leap.nvim) provide cursor movement based
on two characters pattern search. You can go to anywhere with two character search. This action is trigger by key `s`
(forward search) and `S` (backward search). Trigger search mode by `s/S`, then input two character
you want to jump into. Use `gs` to start all window search.

![leap-showcase](https://github.com/ggandor/leap.nvim/raw/media/showcase.gif?raw=true)

> Press `sab` to jump to any words that contains `ab`, if there are multiple word contains pattern `ab`,
> then input the green label attach after it to select.

## Character match based cursor movement

Sometime you might want to jump between text pair, like from character `(` to next `)`. You can use key `,` to trigger this action.
Currently it support `()`, `[]`, `<>`, `{}`. If you are writing code, it even support `if/end`, `function/end`, `if/else`......
You can see [treesitter](./treesitter.md) section to learn how to enable this feature.
