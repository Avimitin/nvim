# Keymaps

<!-- vim-markdown-toc GFM -->

  * [Tree view](#tree-view)
  * [Basic keys](#basic-keys)
    * [Cursor](#cursor)
    * [View](#view)
    * [editing](#editing)
    * [command](#command)
    * [Copy and Paste](#copy-and-paste)
    * [buffer line](#buffer-line)
    * [Windows manage](#windows-manage)
    * [Tab navigation](#tab-navigation)
* [Modify colorscheme](#modify-colorscheme)
* [File Explorer](#file-explorer)
* [lighspeed](#lighspeed)
* [neovim-lspconfig](#neovim-lspconfig)
* [Auto align](#auto-align)
* [Golang](#golang)
* [Git](#git)
  * [Neogit](#neogit)
  * [lazygit](#lazygit)
* [Symbols](#symbols)
* [vim-surround](#vim-surround)
* [wildfire](#wildfire)
* [Rust](#rust)
* [CPP](#cpp)
* [Debug](#debug)
* [Markdown](#markdown)
  * [Snippets](#snippets)
  * [Preview](#preview)
  * [Toc](#toc)
  * [Table align](#table-align)
  * [Others](#others)
* [vim-commentary](#vim-commentary)
* [FTerm](#fterm)
* [Autopairs](#autopairs)
  * [`<CR>`](#cr)
* [Telescope](#telescope)
* [Suda](#suda)
* [Hexokinase](#hexokinase)
* [Neoterm](#neoterm)
* [Vim Rooter](#vim-rooter)

<!-- vim-markdown-toc -->
## Tree view

```text
.
├── ginit.vim       // Settings for nvim-qt
├── init.lua        // Configuration entry
└── lua
   ├── config/      // All the plugins configuration
   ├── colors.lua   // Where I set up my colorscheme
   ├── commands.lua // Some user define commands
   ├── keymap.lua   // keymaps
   ├── options.lua  // neovim editor options and third party GUI options
   ├── plug.lua     // All the plugins
   └── utils.lua    // Some functions I don't want to write it twice

6 directories, 23 files
```

## Basic keys

leader key is `<Space>`.

### Cursor

```text
    ^
    k
 < h l >
    j
    v
```

| key       | function                      |
| ---       | --------                      |
| H         | Move to the front of the line |
| L         | Move to the end of the line   |
| K         | Scroll up                     |
| J         | Scroll down                   |
| W         | Move back 5 words             |
| B         | Move forward 5 words          |
| `<Alt-;>` | Esc                           |

### View

| keymap     | function                      | note                |
| ------     | --------                      | ---                 |
| `Ctrl` + c | put cursor line to the middle | only at insert mode |

### editing

| keymap      | function |
| ------      | -------- |
| `Ctrl` + z  | undo     |
| `Ctrl` + r  | redo     |
| `;` + w     | save     |

### command

In normal mode, you can use `;` to trigger command, no more `Shift` key now.
For example, you will need to press `Shift + ; + q + Enter` to quit neovim,
but now `; + q + Enter` is only needed.

### Copy and Paste

| keymap       | function                                              |
| ------       | --------                                              |
| `<Ctrl-y>`   | copy to system clipboard (select text at visual mode) |
| `<Ctrl-p>`   | paste from system clipboard                           |

### buffer line

| keymap      | function                                     |
| ------      | --------                                     |
| `,`         | move to next tab                             |
| `.`         | move to front tab                            |
| `<Alt> + >` | move current tab to next tab                 |
| `<Alt> + <` | move current tab to front tab                |
| `<Ctrl-c>`  | open pick mode and close the selected window |
| `<Alt-q>`   | close the current buffer                     |

### Windows manage

| keymap            | function                            |
| ------            | --------                            |
| `<Ctrl>+w`+`jhkl` | move cursor to `down/left/up/right` |
| `arrow`           | Resize windows in normal mode       |

### Tab navigation

| keymap           | function       | docs                                   |
| --------------   | -------------- | --------------                         |
| `<CTRL-t> + h/l` | tab naviagate  | h switch to previous, l switch to next |
| `<CTRL-t> + n`   | new tab        | -                                      |

# Modify colorscheme

Now there are
[neovim-deus](https://github.com/Avimitin/neovim-deus) and
[neovim-ayu](https://github.com/Shatur/neovim-ayu) can be used.

You can modify the valuable `theme` in `colors.lua` like:

```lua
local theme = "ayu" -- <- modify this valuable
```

If you want to add the theme you want, follow the below instruction.

1. Add the theme plugin in the plug.lua.

Search the valuable and add you plugin into the curly brackets:

```lua
-- neovim color theme
use {
    'Avimitin/neovim-deus',
    'Shatur/neovim-ayu',
    'YOUR THEME',
}
```

2. Add the configuration for this theme in the colors.lua

Assuming that the name of your theme call "nord".

```lua
local theme = "nord"

local function nord_setup()
  -- Put your configuration here
  -- If you don't have configuration for the theme,
  -- leave it blank here.
end

local theme_opt = {
  ["nord"] = nord_setup,
}
```

You can see the example in the [colors.lua](../lua/colors.lua) file.

# File Explorer

| keymap        | function                                 |
| ------        | --------                                 |
| tt            | open nvim tree explorer                  |
| tr            | refresh nvim tree explorer               |
| `<LEADER>+o`  | open nnn file manager(install nnn first) |

# lighspeed

![image](https://raw.githubusercontent.com/ggandor/lightspeed.nvim/media/intro_img2_incremental_labeling.gif) 

| keymap | function                 |
| ---    | ---                      |
| `s/S`  | search character to jump |

Please read the lightspeed
[README](https://github.com/ggandor/lightspeed.nvim#-usage) for details.


# neovim-lspconfig 

| keymap             | function                     |
| ------             | --------                     |
| `[d/]d`            | Jump around diagnostic       |
| `LEADER` + e       | show diagnostic              |
| `LEADER + h`       | show help message            |
| gd                 | go to definition             |
| gD                 | go to declaration            |
| `<leader>` + r + n | rename variable under cursor |

Check [keymap.lua](../lua/keymap.lua) for more keymaps.

Use the command `:LspInstall` to install lsp you want. Follow the instruction from
[nvim-lsp-installer](https://github.com/williamboman/nvim-lsp-installer).

# Auto align

<img style="max-width: 500px; max-height: 500px" 
src="https://raw.githubusercontent.com/junegunn/i/master/easy-align/equals.gif" />

| keymap       | function                                                      |
| ------       | --------                                                      |
| `:EasyAlign` | In virsual mode or normal mode this will active align plugin. |
| `<leader>+e` | Activate easy align when in visual mode                       |

Read [junegunn/vim-easy-align](https://github.com/junegunn/vim-easy-align) for more.

# Golang

| keymap | function                            |
| ---    | ---                                 |
| got    | go test function under cursor |
| gor    | go run  current  `.go` file   |

Tricks: using `:GoRename` to renamed variable or struct name, can automatically changed all
the name of references. Check out [fatih/vim-go](https://github.com/fatih/vim-go) for more.

# Git

| keymap           | function                     |
| ---              | ---                          |
| gis              | stage changes                |
| gip              | preview changes              |
| giu              | undo stage                   |
| gib              | git blame                    |
| gir              | reset changes                |
| gin              | next hunk                    |
| gim              | previous hunk                |

You can also run git command like what you do in the terminal. For example, if you
want to see the log, just run `:Git log` or `:G log`. If you want to see the blame,
run `:Git/G blame`. If you want to rebase, you can run `:G rebase -i HEAD~n`. This
command are all enhanced by plugin vim-fugitive.

## Neogit

![neogit](https://user-images.githubusercontent.com/32014449/109874516-8042af00-7c6f-11eb-8afc-65ef52448c7a.png)

Neogit is an vim alternative of magit. Press `<Space>ng` to activate it.

## lazygit

![image](https://raw.githubusercontent.com/jesseduffield/lazygit/assets/staging.gif) 

Press `<Space>lg` to activate it. You need to install
[lazygit](https://github.com/jesseduffield/lazygit).

# Symbols

![image](https://raw.githubusercontent.com/simrat39/rust-tools-demos/master/symbols-demo.gif) 

Run command `:SymbolOutline` to activate it.

# vim-surround

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

# wildfire

![image](https://raw.githubusercontent.com/gcmt/wildfire.vim/master/_assets/preview.gif) 

plugin to help you quick select surrounded object

<img style="max-width: 500px; max-height: 500px" 
src="https://raw.githubusercontent.com/gcmt/wildfire.vim/master/_assets/preview.gif" />

Press `<Return>` to select text.

# Rust

![img](https://raw.githubusercontent.com/simrat39/rust-tools-demos/master/rust-tools-debug.gif) 

~~Use command `:LspInstall rust` to automatically configured up
[rust-ananlyzer](https://rust-analyzer.github.io/) as lsp server.~~

> The rust-tools.nvim will set up rust analyzer itself, so it's recommended
> not to configured Rust LSP yourself. You just need to install rust-analyzer.

Also you will have extra command to help you coding in Rust. See
[rust-tool.nvim](https://github.com/simrat39/rust-tools.nvim/) for more.

Use keymap `<LEADER>ra` to activate `:RustHoverActions`.

# CPP

If you are using cmake, use below command to set up your project.

```bash
cmake -BDebug . -DCMAKE_EXPORT_COMPILE_COMMANDS=1
ln -s Debug/compile_commands.json .
```

# Debug

- CPP

First you need to
[build](https://github.com/mfussenegger/nvim-dap/wiki/Debug-Adapter-installation#building-lldb-vscode)
or install lldb-vscode.

> If you are Arch Linux user, run: `pacman -S lldb`

Then build your cpp program with `-g` arguments:

```bash
clang++ --std=c++17 -Wall -g -o debug.app example.cc
```

Then add breakpoint in your neovim with command `:DapBreakpoint` or `DapBp`.

Open the debug UI with command `:DapUiToggle`.

And finally run the command `:DapContinue` or `:DapC` to start debugging. You
will need to add the executable path you built above.

- Rust

Run command `:RustHoverAction` or keymap `<Space>ra`, then choose debug.

Open the debug UI with command `:DapUiToggle`.

---

For details or more setting please read the README from this project.

- Dap(The debug core plugin): https://github.com/mfussenegger/nvim-dap
- Dap-UI(Debug UI): https://github.com/rcarriga/nvim-dap-ui
- rust-tools.nvim(Rust Utilities): https://github.com/simrat39/rust-tools.nvim/

# Markdown

## Snippets

```text
code
codeblock
h[1-5]
img
link
table
```

## Preview

Press `:MarkdownPreview` command to activate preview, change the default browser
and port in lua/config/mkdp.lua.

## Toc

Go to the line you want to generate TOC, then use the command `:GenTocGFM` to generate
TOC that suitable for GitHub repository. Use the command `:GenToGitLab` to generate TOC
that suitable for GitLab repository.

## Table align

Select lines at normal mode, press `:EasyAlign *|` can align your table.

## Others

I've add the vim-markdown to provide some handy markdown tools, read the
[README](https://github.com/plasticboy/vim-markdown#mappings)
file for details.

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

# Telescope

![screenshot](https://raw.githubusercontent.com/siduck76/dotfiles/all/rice%20flex/tel.png) 

```text
<leader>tf Telescope find_files
<leader>tg Telescope live_grep
```

# Suda

When you open a file which have no right to access, you can use this plugin to
get root priviledge.

```sh
:SudaWrite (Write with sudo)
:SudaRead  (Read with sudo)
```

# Hexokinase

![Hexokinase](https://user-images.githubusercontent.com/21000943/64057161-91d16500-cb67-11e9-83ab-535ad2489c5a.png)

A plugin which can show colors. Use command `:HexokinaseToggle` to activate it.

# Neoterm

![image](https://cloud.githubusercontent.com/assets/120483/8921869/fe459572-34b1-11e5-93c9-c3b6f3b44719.gif)

Use command `:T`, `:Tkill`, `:Tclose`, `:Tmap` to activate it.

# Vim Rooter

A plugins that can automatically change your directory to the project root.
It will find `.git` and `Cargo.toml` and mark their pwd as root pattern. It
will be automatically triggered after opening the neovim.

You can also run command `:Rooter` to trigger it.
