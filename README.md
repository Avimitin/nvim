# nvim config with Gopher behavior

![User Interface Preview](https://cdn.jsdelivr.net/gh/Avimitin/PicStorage/pic/20210228180603.png)

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

* [Before...](#before)
* [Dependence](#dependence)
	* [Install neovim](#install-neovim)
	* [Python](#python)
	* [nodejs](#nodejs)
	* [Install nerdfont and powerline font](#install-nerdfont-and-powerline-font)
	* [Check health](#check-health)
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
* [Coding](#coding)
	* [COC.NVIM](#cocnvim)
	* [Auto align](#auto-align)
	* [Golang](#golang)
	* [Git](#git)
	* [Vista](#vista)
	* [lazygit](#lazygit)
	* [vim-surround](#vim-surround)
	* [wildfire](#wildfire)

<!-- vim-markdown-toc -->
</details>

## Before...

You should fork this configuration to your personal repository and clone it to `~/.config`. 
In this way you can store your customization and make your configuration easy to migrate.

For easily changed setting and path, i decided to maintain two version of init.vim.
To get specific machine version run command below.

```bash
# linux or mac user
git clone -b linux https://github.com/avimitin/nvim

# windows user
git clone -b windows https://github.com/avimitin/nvim
```

## Dependence

### Install neovim

> IMPORTANT: nightly 0.5.0 version is needed, because of the pop up windows and remote
support. So Ubuntu user is recommended to add the unstable ppa source to have full plugin
support.

- Windows

Windows user just download [nvim-qt](https://github.com/neovim/neovim/releases/tag/nightly) 
and extract it to the folder you want, then add the bin directory to system path.

- Linux

Linux user can build up neovim yourself or download it from your package manager.

```bash
# if you are an Ubuntu user
# removed out of dated version
sudo apt remove neovim
sudo add-apt-repository ppa:neovim-ppa/unstable
sudo apt-get update
sudo apt install neovim

# OR if you are a Arch user
yay -S neovim-nightly-bin
```

### Python

Python is really important as it help me using file manager like ranger or file finder like fzf.
So it is MUSTED to install python dependece.

Windows user can install python from 
[Microsoft Store](https://www.microsoft.com/en-us/p/python-3/9nblggh083nz?activetab=pivot:overviewtab) 
or from official website.

> Windows user need to first get into Python install folder. Copy python.exe and renamed it to 
python3.exe. This can avoid python3 not found error.

Linux user just use your package manager to install.

```bash
# Ubuntu
sudo apt install python3 python3-pip
# Arch
yay -S python3 python3-pip
```

After python3 install you need to install `pynvim`

```bash
pip3 install pynvim
```

### nodejs

Nodejs is needed for coc.nvim and it's plugin.
It is really important if you want to use auto-complete, language server for diagnostic 
and some useful plugins like vscode plugin.

Using nvm to manage node and nvm can avoid permission problem on linux. 
You should follow the [nvm install guide](./docs/nodejs_install.md) to 
install nvm before you install nodejs.

Windows user can just download install package from Official Website and 
follow the guide to install.

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

open your vim and input command to check dependence is all install or not.

```vim
:checkhealth
```

## Keymap definition

### Basic keys

- Cursor

```text
    ^
    u
 < h k >
    j
    v
```

| key | function                      |
| --- | --------                      |
| h   | left                          |
| k   | right                         |
| u   | up                            |
| j   | down                          |
| H   | Move to the front of the line |
| K   | Move to the end of the line   |
| U   | Move up 5 lines               |
| J   | Move down 5 lines             |
| W   | Move back 5 words             |
| B   | Move forward 5 words          |
| l   | move from word to word        |
| L   | move back from word to word   |
| jj  | Esc at insert mode            |

- View

| keymap     | function                      |
| ------     | --------                      |
| `Ctrl` + c | put cursor line to the center |

- editing

| keymap     | function |
| ------     | -------- |
| `Ctrl` + z | undo     |
| `Ctrl` + r | redo     |

## More useful stuff

### LEADER key

leader key is `<Space>` .

### Save and Quit

| keymap                  | function      |
| ------                  | --------      |
| `LEADER`+s              | save          |
| `LEADER`+q              | save and quit |
| `Ctrl`+`Alt`+`shift`+ q | force quit    |
| `Ctrl` + q              | quit all file |
| Q                       | just quit     |

### Copy and Paste

| keymap       | function                                              |
| ------       | --------                                              |
| `leader` + y | copy to system clipboard (select text at visual mode) |
| `leader` + p | paste from system clipboard                           |

### Tab page

| keymap | function                      |
| ------ | --------                      |
| tu     | create new tab                |
| tk     | move to next tab              |
| th     | move to front tab             |
| tmk    | move current tab to next tab  |
| tmh    | move current tab to front tab |

### Windows manage

| keymap   | function                            |
| ------   | --------                            |
| s+`uhjk` | move cursor to `up/left/down/right` |
| `arrow`  | Resize windows in normal mode       |

### File Exploer

| keymap | function                           |
| ------ | --------                           |
| tt     | open coc explorer                  |
| ?      | open coc explorer guide            |
| R      | open ranger (install ranger first) |

### File finder

<img style="max-width: 500px; max-height: 500px" 
src="https://camo.githubusercontent.com/01c738192dc98e59cc139b2591f3c43b6dc75d06b5b57ac097c4c1acd8e8f160/68747470733a2f2f6a657373656c656974652e636f6d2f75706c6f6164732f706f7374732f322f7461672d66696e6465722d6f70742e676966"/>

| keymap       | function                   |
| ---          | ---                        |
| `<LEADER>+f` | activate fuzzy file finder |

### terminal 

press `ctrl+\` to open terminal

## Coding

### COC.NVIM

<img style="max-width: 500px; max-height: 500px" 
src="https://user-images.githubusercontent.com/251450/55285193-400a9000-53b9-11e9-8cff-ffe4983c5947.gif"/>

| keymap                 | function                                                     |
| ------                 | --------                                                     |
| `LEADER` + -/=         | Move to next of front diagnostic                             |
| `LEADER` + d           | Coc diagnostic                                               |
| `LEADER` + h           | Open definition                                              |
| gd                     | go to definition                                             |
| `LEADER` + `Backspace` | go back to last file (pretty useful when jump between files) |

Using `:CocInstall` to install plugin you want.
check out [coc.nvim wiki](https://github.com/neoclide/coc.nvim/wiki) for more.

### Auto align

<img style="max-width: 500px; max-height: 500px" 
src="https://raw.githubusercontent.com/junegunn/i/master/easy-align/equals.gif" />

| keymap                     | function                                                      |
| ------                     | --------                                                      |
| `:EasyAlign<CR>+<KEYWORD>` | In virsual mode or normal mode this will active align plugin. |

Tricks: select lines at normal mode, press `:EasyAlign *|` can format markdown file.
Checkout [junegunn/vim-easy-align](https://github.com/junegunn/vim-easy-align) for more.

### Golang

| keymap | function                      |
| ---    | ---                           |
| gt     | go test function under cursor |
| gr     | go run current `.go` file     |
| gi     | go organize import            |

Tricks: using `:GoRename` to renamed variable or struct name, can automatically changed all
the name of references. Check out [fatih/vim-go](https://github.com/fatih/vim-go) for more.

### Git

| keymap          | function        |
| ---             | ---             |
| `<LEADER>` + gs | stage changes   |
| `<LEADER>` + gp | preview changes |
| `<LEADER>` + gu | undo changes    |

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
| CS   | find and replace symbol                                                     |

"HELLO" -> press `CS"'` change double quote to single quote -> 'HELLO'

### wildfire

plugin to help you quick select surrounded object

<img style="max-width: 500px; max-height: 500px" 
src="https://raw.githubusercontent.com/gcmt/wildfire.vim/master/_assets/preview.gif" />

Press enter to select text.

> using wildfire and vim-surround can quickly select and changed object symbol.

