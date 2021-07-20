# nvim config with Gopher convention

![User Interface Preview](./image/screenshot.png)

## Intro

**[English](./README.md) (You are here)**
|
[中文](docs/README_CN.md)

This project is inspired by [theniceboy/nvim](https://github.com/theniceboy/nvim). 
Changed Colemak keyboard style to qwerty keyboard style and deleted useless stuff.

## Guide

<details>
<summary>Click to open</summary>
<!-- vim-markdown-toc GFM -->

* [TODO](#todo)
* [Before...](#before)
* [Dependence](#dependence)
  * [neovim](#neovim)
  * [Python](#python)
  * [Node.js](#nodejs)
  * [Install nerdfont and powerline font](#install-nerdfont-and-powerline-font)
  * [Check health](#check-health)
  * [FAQ](#faq)
* [Keymap definition](#keymap-definition)
  * [Basic keys](#basic-keys)
* [More useful stuff](#more-useful-stuff)
  * [LEADER key](#leader-key)
  * [Save and Quit](#save-and-quit)
  * [Copy and Paste](#copy-and-paste)
  * [Tab page](#tab-page)
  * [Windows manage](#windows-manage)
  * [File Exploer](#file-exploer)
  * [File finder](#file-finder)
  * [terminal](#terminal)
  * [EasyMotion](#easymotion)
* [Coding](#coding)
  * [COC.NVIM](#cocnvim)
  * [Auto align](#auto-align)
  * [Golang](#golang)
  * [Git](#git)
  * [Vista](#vista)
  * [lazygit](#lazygit)
  * [vim-surround](#vim-surround)
  * [wildfire](#wildfire)
  * [Rust](#rust)
* [Markdown](#markdown)
  * [Preview](#preview)
  * [Toc](#toc)
  * [Table align](#table-align)
* [far.vim](#farvim)
* [smoothie](#smoothie)
* [vim-after-object](#vim-after-object)
* [vim-commentary](#vim-commentary)
* [Snippets](#snippets)
  * [markdown](#markdown-1)
* [AsyncRun](#asyncrun)

<!-- vim-markdown-toc -->
</details>

## TODO

- [ ] Replace coc.nvim with built-in lsp

## Before...

Fork this configuration to your repository and clone it to `~/.config` for storing
your own customization. Also, it can help you stay away from version conflict.

To make it easier to maintain, I decided to used two different branches of the
configuration file.

> `IMPORTANT NOTES`: Windows branches is no more maintained. Please try WSL
> for running the neovim.

```bash
# linux or mac user
git clone https://github.com/avimitin/nvim ~/.config/nvim
```

## Dependence

### neovim 

> `IMPORTANT NOTES`: Please build the neovim from github, 
> you will need at least `NVIM v0.5.0-dev+1411-gb28d458f8`.

- Linux

Linux user must build neovim manually yourself, or download from your package 
manager if available.

```bash
# if you are an Ubuntu user
# removed out of dated version
sudo apt remove neovim
sudo add-apt-repository ppa:neovim-ppa/unstable
sudo apt-get update
sudo apt install neovim

# OR if you are a Arch user
yay -S neovim-git
```

### Python

Once Python is installed, run the following command to install dependencies.

```console
python3 -m pip install pynvim

# Also, Arch Linux users can install it from AUR
yay -S python-pynvim
```

### Node.js

Linux users should follow the [nvm install guide](./docs/nodejs_install.md) to
install Node.js version manager. Don't install node and npm from your package manager.

After install:

```bash
# install nvm first
npm install -g neovim
```

### Install nerdfont and powerline font

It's highly recommended to install [nerdfont](https://www.nerdfonts.com/font-downloads) 
for impressive icon support

Also for powerline icon support you should install
[powerline/font](https://github.com/powerline/fonts)

### Check health

Open your neovim and input following command to check if the dependence is all installed or not.

```vim
:checkhealth
```

### FAQ

- vim-hexokinase

you may met the error: 

```text
vim-hexokinase needs updating. 
Run `make hexokinase` in project root. 
See `:h hexokinase-installation` for more info.`
```

This plugin is used for generating color from code. For example, it will generate 
black color after `#000000`.

Get into your nvim config folder, input the following command.

```bash
cd plugged/vim-hexokinase
make hexokinase
```

- colorscheme

1. The neovim have no colorscheme, what happen?
2. The neovim pop up an error message which said `deus not found`.

Please build the latest neovim which support the `runtime Lua` features.

Another workaround is to using old deus version.

For more detail please refer to [Avimitin/neovim-deus](https://github.com/Avimitin/neovim-deus)

## Keymap definition

### Basic keys

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
| h   | left                          |
| l   | right                         |
| k   | up                            |
| j   | down                          |
| H   | Move to the front of the line |
| L   | Move to the end of the line   |
| K   | Move up 5 lines               |
| J   | Move down 5 lines             |
| N   | Move back 5 words             |
| B   | Move forward 5 words          |
| n   | move from word to word        |
| b   | move back from word to word   |
| jj  | Esc at insert mode            |
| i   | start insert mode             |

- View

| keymap     | function                                       | doc                 |
| ------     | --------                                       | ---                 |
| `Ctrl` + c | put the line which the cursor is to the center | only in insert mode |

- editing

| keymap      | function |
| ------      | -------- |
| `Ctrl` + z  | undo     |
| `Ctrl` + r  | redo     |
| `Ctrl` + s  | save     |

## More useful stuff

### LEADER key

leader key is `<Space>` .

### Save and Quit

| keymap                  | function                |
| ------                  | --------                |
| `LEADER`+s              | save                    |
| `LEADER`+q              | save and quit           |
| `Ctrl`+`Alt`+`shift`+ q | force quit              |
| `Ctrl` + q              | quit                    |
| `Alt` + q               | quit buffer (tab above) |

### Copy and Paste

| keymap       | function                                              |
| ------       | --------                                              |
| `leader` + y | copy to system clipboard (select text at visual mode) |
| `leader` + p | paste from system clipboard                           |

### Tab page

| keymap      | function                      |
| ------      | --------                      |
| `,`         | move to next tab              |
| `.`         | move to front tab             |
| `<Alt> + >` | move current tab to next tab  |
| `<Alt> + <` | move current tab to front tab |

Also they are clickable: 

![img](https://raw.githubusercontent.com/romgrk/barbar.nvim/master/static/click.gif)

### Windows manage

| keymap   | function                            |
| ------   | --------                            |
| s+`jhkl` | move cursor to `up/left/down/right` |
| `arrow`  | Resize windows in normal mode       |

### File Exploer

| keymap             | function                                               |
| ------             | --------                                               |
| tt                 | open coc explorer                                      |
| ?                  | open coc explorer guide                                |
| ~~R~~ (deprecated) | open ranger (install ranger first)                     |
| `<LEADER>+o`       | open a new tab and nnn file manager(install nnn first) |
| `<ctrl>+n`         | open nnn file manager                                  |

### File finder

<img style="max-width: 500px; max-height: 500px" 
src="https://camo.githubusercontent.com/01c738192dc98e59cc139b2591f3c43b6dc75d06b5b57ac097c4c1acd8e8f160/68747470733a2f2f6a657373656c656974652e636f6d2f75706c6f6164732f706f7374732f322f7461672d66696e6465722d6f70742e676966"/>

| keymap       | function                   |
| ---          | ---                        |
| `<LEADER>+f` | activate fuzzy file finder |

### terminal 

press `ctrl+\` to open terminal

### EasyMotion

| keymap       | function             |
| ---          | ---                  |
| `u`          | activate easymotion  |
| `<leader>+j` | easymotion jump line |
| `<leader>+k` | easymotion jump line |

Example usage:

Input `u` in normal mode and input two character of the keyword. Then input the red character, the
cursor will jump to that line.

For more usage checkout: [ vim-easymotion ](https://github.com/easymotion/vim-easymotion)

## Coding

### COC.NVIM

<img style="max-width: 500px; max-height: 500px" 
src="https://user-images.githubusercontent.com/251450/55285193-400a9000-53b9-11e9-8cff-ffe4983c5947.gif"/>

| keymap                 | function                                                     |
| ------                 | --------                                                     |
| `LEADER` + -/=         | Move to next of front diagnostic                             |
| `LEADER` + d           | Coc diagnostic                                               |
| `LEADER` + h           | Open  help                                                   |
| gd                     | go to definition                                             |
| `LEADER` + `Backspace` | go back to last file (pretty useful when jump between files) |
| `Ctrl + c`             | open command menu                                            |

Using `:CocInstall` to install plugin you want.
check out [coc.nvim wiki](https://github.com/neoclide/coc.nvim/wiki) for more.

- Rename variable

![img](./image/coc_rename.webp)

| keymap             | function                     |
| ---                | ---                          |
| `<leader>` + r + n | rename variable under cursor |

### Auto align

<img style="max-width: 500px; max-height: 500px" 
src="https://raw.githubusercontent.com/junegunn/i/master/easy-align/equals.gif" />

| keymap                     | function                                                      |
| ------                     | --------                                                      |
| `:EasyAlign<CR>+<KEYWORD>` | In virsual mode or normal mode this will active align plugin. |

Checkout [junegunn/vim-easy-align](https://github.com/junegunn/vim-easy-align) for more.

### Golang

| keymap | function                      |
| ---    | ---                           |
| got     | go test function under cursor |
| gor     | go run current `.go` file     |

Tricks: using `:GoRename` to renamed variable or struct name, can automatically changed all
the name of references. Check out [fatih/vim-go](https://github.com/fatih/vim-go) for more.

### Git

| keymap | function        |
| ---    | ---             |
| gis    | stage changes   |
| gip    | preview changes |
| giu    | undo changes    |

### Vista

This plugin can list all the variable or type definition in the current file

<img style="max-width: 500px; max-height: 500px" 
src="https://user-images.githubusercontent.com/8850248/56469894-14d40780-6472-11e9-802f-729ac53bd4d5.gif" />

package `ctag` is needed.

Press `<LEADER>` + v to activate it.

### lazygit

lazygit is a simple and easy to learn and use terminal git manager

<img style="max-width: 500px; max-height: 500px" 
src="https://raw.githubusercontent.com/jesseduffield/lazygit/assets/rebase.gif" />

press `ctrl+g` to activate it. (Install lazygit first)

### vim-surround

[vim-surround](https://github.com/tpope/vim-surround) 
can help you do all the 'surround' stuff easyily。

<img style="max-width: 500px; max-height: 500px" 
src="https://camo.githubusercontent.com/334f5a06cbee4141889dfdf18a7c51a0ea408edb4d79f4dbe77e4d8b937d5a0b/68747470733a2f2f74776f2d77726f6e67732e636f6d2f696d6167652f737572726f756e645f76696d2e676966" />

| key  | function                                                                    |
| ---- | ------------------------------------------------------------                |
| S    | press big S when you selected text, and press a symble you want to surround |
| cs   | find and replace symbol                                                     |

"HELLO" -> press `CS"'` change double quote to single quote -> 'HELLO'

### wildfire

plugin to help you quick select surrounded object

<img style="max-width: 500px; max-height: 500px" 
src="https://raw.githubusercontent.com/gcmt/wildfire.vim/master/_assets/preview.gif" />

Press enter to select text.

> using wildfire and vim-surround can quickly select and changed object symbol.


### Rust

Must install [rust-analyzer](https://rust-analyzer.github.io/manual.html), rust lsp is configured 
in `coc-setting.json`

> This plugin is not test in Windows system yet. So there is no change on windows branch.

## Markdown

### Preview

Press `:MarkdownPreview` command to activate preview, change the default browser
and port in core/plugins.vim.

### Toc

Go to the line you want to generate TOC, then use the command `:GenTocGFM` to generate
TOC that suitable for GitHub repository. Use the command `:GenToGitLab` to generate TOC
that suitable for GitLab repository.

### Table align

Select lines at normal mode, press `:EasyAlign *|` can align your table.

## far.vim

![img](https://cloud.githubusercontent.com/assets/9823254/20861878/77dd1882-b9b4-11e6-9b48-8bc60f3d7ec0.gif)

Press `<ctrl>+f` to open far.vim which can help you search keyword from multiple file.

You will need the [silversearcher](https://github.com/ggreer/the_silver_searcher)

## smoothie

| keymap     | function                 |
| `<Ctrl>+d` | smoothly scroll downward |
| `<Ctrl>+u` | smoothly scroll upward   |

## vim-after-object

```text
# va=  visual after =
# ca=  change after =
# da=  delete after =
# ya=  yank after =
apple = 'juicy'
```

## vim-commentary

Use gcc to comment out a line (takes a count), gc to comment out the target
of a motion (for example, gcap to comment out a paragraph), gc in visual mode
to comment out the selection, and gc in operator pending mode to target a
comment. You can also use it as a command, either with a range like :7# 7Commentary,
or as part of a :global invocation like with :g/TODO/Commentary. That's it.

## Snippets

Input the keyword below and press tab to switch between waiting key. Once
you select the keyword, press `<Enter>` to trigger completion. Also, 
you can press `<Ctrl>+j` to expand snippets. Press `<tab>` jump from visual text.
Press `<Ctrl>+k` to jump back visual text.

### markdown

| shortcut   | behaves                                         |
| :--------- | :--------                                       |
| `h[1-5]`   | create H1-H5 section                            |
| `*`        | italic                                          |
| `**`       | bold                                            |
| `/*`       | comment                                         |
| `link`     | link to something                               |
| `img`      | link to image                                   |
| `ico`      | inline code                                     |
| `bco`      | code block                                      |
| `tb`       | create a table with [1-9] line and [1-9] column |
| `head`     | create a front-matter with my name              |

Changed the front-matter user's name in `./UltiSnips/markdown.snippets`.

## AsyncRun

Press `<Ctrl>+\` and input command to execute command asynchronously 
and open a quickfix windows.
