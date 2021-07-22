# Keymaps

I won't repeat the default key here, I'll just mention some of the key which
modified.

## Basic keys

- Cursor

```text
    ^
    k
 < h l >
    j
    v
```

| key | function                      |
| --- | --------                      |
| H   | Move to the front of the line |
| L   | Move to the end of the line   |
| K   | Move up 5 lines               |
| J   | Move down 5 lines             |
| W   | Move back 5 words             |
| B   | Move forward 5 words          |
| jj  | Esc at insert and visual mode |
| i   | start insert mode             |

- View

| keymap     | function                      | doc                 |
| ------     | --------                      | ---                 |
| `Ctrl` + c | put cursor line to the middle | only at insert mode |

- editing

| keymap      | function |
| ------      | -------- |
| `Ctrl` + z  | undo     |
| `Ctrl` + r  | redo     |
| `Ctrl` + s  | save     |

## LEADER key

leader key is `<Space>`.

## Save and Quit

| keymap                  | function                |
| ------                  | --------                |
| `LEADER`+s              | save                    |
| `LEADER`+q              | save and quit           |
| `Ctrl` + q              | quit                    |

## Copy and Paste

| keymap       | function                                              |
| ------       | --------                                              |
| `leader` + y | copy to system clipboard (select text at visual mode) |
| `leader` + p | paste from system clipboard                           |

## Tab page

| keymap      | function                                     |
| ------      | --------                                     |
| `,`         | move to next tab                             |
| `.`         | move to front tab                            |
| `<Alt> + >` | move current tab to next tab                 |
| `<Alt> + <` | move current tab to front tab                |
| `<Alt> + x` | open pick mode and close the selected window |

## Windows manage

| keymap   | function                            |
| ------   | --------                            |
| s+`jhkl` | move cursor to `up/left/down/right` |
| `arrow`  | Resize windows in normal mode       |

## File Exploer

| keymap        | function                                 |
| ------        | --------                                 |
| tt            | open coc explorer                        |
| `<LEADER>+o`  | open nnn file manager(install nnn first) |

## File finder

| keymap        | function                   |
| ---           | ---                        |
| `<LEADER>+ff` | activate fuzzy file finder |
| `<LEADER>+fg` | activate live grep         |
| `<LEADER>+fb` | activate buffer finder     |
| `<Ctrl+c>`    | exit                       |

## Hop

| keymap       | function             |
| ---          | ---                  |
| `u`          | activate hop         |
| `<leader>+j` | easymotion jump line |
| `<leader>+k` | easymotion jump line |

Example usage:

Input `u` in normal mode and input two character of the keyword which highlighting. 
Then the cursor will jump to that place.

## neovim-lspconfig 

| keymap       | function               |
| ------       | --------               |
| `[d/]d`      | Jump around diagnostic |
| `LEADER` + e | show diagnostic        |
| `LEADER + h` | show help message      |
| gd           | go to definition       |
| gD           | go to declaration      |

Check [lsp.lua](../lua/plugins/lsp.lua) for more keymaps.

Using `:LspInstall` to install plugin you want.

- Rename variable

| keymap             | function                     |
| ---                | ---                          |
| `<leader>` + r + n | rename variable under cursor |

## Auto align

<img style="max-width: 500px; max-height: 500px" 
src="https://raw.githubusercontent.com/junegunn/i/master/easy-align/equals.gif" />

| keymap                     | function                                                      |
| ------                     | --------                                                      |
| `:EasyAlign<CR>+<KEYWORD>` | In virsual mode or normal mode this will active align plugin. |
| `<leader>+e`               | Activate easy align when in visual mode                       |

Checkout [junegunn/vim-easy-align](https://github.com/junegunn/vim-easy-align) for more.

## Golang

| keymap | function                            |
| ---    | ---                                 |
| got    | go test function under cursor |
| gor    | go run  current  `.go` file   |

Tricks: using `:GoRename` to renamed variable or struct name, can automatically changed all
the name of references. Check out [fatih/vim-go](https://github.com/fatih/vim-go) for more.

## Git

| keymap | function        |
| ---    | ---             |
| gis    | stage changes   |
| gip    | preview changes |
| giu    | undo stage      |
| gib    | git blame       |
| gir    | reset changes   |
| gin    | next hunk       |
| gim    | previous hunk   |

## Symbols

Press `<LEADER>` + l to activate it.

## lazygit

press `ctrl+g` to activate it. (Install lazygit first)

## vim-surround

[vim-surround](https://github.com/tpope/vim-surround) 
can help you do all the 'surround' stuff easyily。

<img style="max-width: 500px; max-height: 500px" 
src="https://camo.githubusercontent.com/334f5a06cbee4141889dfdf18a7c51a0ea408edb4d79f4dbe77e4d8b937d5a0b/68747470733a2f2f74776f2d77726f6e67732e636f6d2f696d6167652f737572726f756e645f76696d2e676966" />

| key  | function                                                                    |
| ---- | ------------------------------------------------------------                |
| S    | press big S when you selected text, and press a symble you want to surround |
| cs   | find and replace symbol                                                     |

"HELLO" -> press `CS"'` change double quote to single quote -> 'HELLO'

## wildfire

plugin to help you quick select surrounded object

<img style="max-width: 500px; max-height: 500px" 
src="https://raw.githubusercontent.com/gcmt/wildfire.vim/master/_assets/preview.gif" />

Press enter to select text.

> using wildfire and vim-surround can quickly select and changed object symbol.

## Rust

Must install [rust-analyzer](https://rust-analyzer.github.io/manual.html), rust lsp is configured 
in `coc-setting.json`

> This plugin is not test in Windows system yet. So there is no change on windows branch.

# Markdown

## Preview

Press `:MarkdownPreview` command to activate preview, change the default browser
and port in core/plugins.vim.

## Toc

Go to the line you want to generate TOC, then use the command `:GenTocGFM` to generate
TOC that suitable for GitHub repository. Use the command `:GenToGitLab` to generate TOC
that suitable for GitLab repository.

## Table align

Select lines at normal mode, press `:EasyAlign *|` can align your table.

# vim-commentary

Use gcc to comment out a line (takes a count), gc to comment out the target
of a motion (for example, gcap to comment out a paragraph), gc in visual mode
to comment out the selection, and gc in operator pending mode to target a
comment. You can also use it as a command, either with a range like :7# 7Commentary,
or as part of a :global invocation like with :g/TODO/Commentary. That's it.

# FTerm

Press `<Ctrl>+\` to open a terminal, press it again to close the window.

