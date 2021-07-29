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

## buffer line

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
| tt            | open nvim tree explorer                  |
| `<LEADER>+o`  | open nnn file manager(install nnn first) |

## Hop

![image](https://camo.githubusercontent.com/e71f83e31fd8950c8a584e28d68a5ca97502d3a57919119fb9e21943cb5ff76c/68747470733a2f2f706861617a6f6e2e6e65742f6d656469612f75706c6f6164732f686f705f63686172325f6d6f64652e676966) 

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

![img](https://raw.githubusercontent.com/lewis6991/media/main/gitsigns_blame.gif) 

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

![image](https://raw.githubusercontent.com/simrat39/rust-tools-demos/master/symbols-demo.gif) 

Press `<LEADER>` + l to activate it.

## lazygit

![image](https://raw.githubusercontent.com/jesseduffield/lazygit/assets/staging.gif) 

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
| ds   | revert to original text                                                     |

"HELLO" -> press `CS"'` change double quote to single quote -> 'HELLO'

## wildfire

![image](https://raw.githubusercontent.com/gcmt/wildfire.vim/master/_assets/preview.gif) 

plugin to help you quick select surrounded object

<img style="max-width: 500px; max-height: 500px" 
src="https://raw.githubusercontent.com/gcmt/wildfire.vim/master/_assets/preview.gif" />

Press enter to select text.

> using wildfire and vim-surround can quickly select and changed object symbol.

## Rust

![img](https://raw.githubusercontent.com/simrat39/rust-tools-demos/master/rust-tools-debug.gif) 

Use command `:LspInstall rust` to automatically configured up [rust-ananlyzer](https://rust-analyzer.github.io/)
as lsp server.

Also you will have extra command to help you coding in Rust. See
[rust-tool.nvim](https://github.com/simrat39/rust-tools.nvim/) for more.

## CPP

Use command `:LspInstall cpp` to automatically configured up clangd as lsp server.

Use command `:Neoformat` to format your cpp file.
[clang-format](https://clang.llvm.org/docs/ClangFormat.html) is needed. It will read `.clang-format`
from your project root.

# Markdown

## Snippets

```text
code
codeblock
h[1-5]
...
```

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

![image](https://user-images.githubusercontent.com/24727447/113905276-999bc580-97f0-11eb-9c01-347de0ff53c9.png) 

Press `<Ctrl>+\` to open a terminal, press it again to close the window.

# Autopairs

## `<CR>`

```text
Before        Input         After
------------------------------------
{|}           <CR>          {
                              |
                            }
------------------------------------
```

## Fastwrap

```text
Before        Input                    After
--------------------------------------------------
(|foobar      \e then press $        (|foobar)
(|)(foobar)   \e then press q       (|(foobar))
```

# Telescope

![screenshot](https://raw.githubusercontent.com/siduck76/dotfiles/master/rice%20flex/telmedia.png) 

```text
<leader>ff Telescope find_files
<leader>fp Telescope media_files
<leader>fg Telescope live_grep
<leader>fb Telescope buffers
<leader>fh Telescope help_tags
```

