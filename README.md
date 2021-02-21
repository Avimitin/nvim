# nvim config with Gopher behavior

![](https://cdn.jsdelivr.net/gh/Avimitin/PicStorage/pic/20210221171519.png)

[中文版](docs/README_CN.md)

## Intro

This project is inspire by [theniceboy/nvim](https://github.com/theniceboy/nvim). Change Colemak keyboard style to qwerty keyboard style and minify useless stuff.

## Before...

You should fork this configuration to your personal repository and clone it to `~/.config`. In this way you can store your customization and make your configuration easy to migrate.

## Dependence

- Install neovim

```bash
sudo apt install neovim python3-pip ranger

pip3 install pynvim
# install nvm first
npm install -g neovim
```

[nvm install guide](./docs/nodejs_install.md)

- Install nerdfont and powerline font

It's highly recommended to install [nerdfont](https://www.nerdfonts.com/font-downloads) for impressive icon support

Also for powerline icon support you should install [powerline/font](https://github.com/powerline/fonts)

- Check health

```vim
:CheckHealth
```

- Install plugin

```vim
:PlugInstall
```

## Information about my keymap definition

### Basic keys

- Cursor

```text
    ^
    u
 < h k >
    j
    v
```

| key | function |
| --- | -------- |
|  h  | cursor left |
|  k  | cursor right|
|  u  | cursor up   |
|  j  | cursor down |
|  H  | Move to the front of the line |
|  k  | Move to the end of the line   |
|  U  | Move up 5 lines |
|  J  | Move down 5 lines |
|  W  | Move back 5 words |
|  B  | Move forward 5 words |
| `Ctrl` + h | In insert mode this will move cursor to the front of the line |
| `Ctrl` + k | In insert mode this will move cursor to the end of line |
| ; + e | Esc at insert mode |

- View

| keymap | function |
| ------ | -------- |
| `Ctrl` + c | In insert mode this will put cursor line to the center |

- editing

| keymap | function |
| ------ | -------- |
| `Ctrl` + z | undo |
| `Ctrl` + r | redo |

## More useful stuff

- LEADER key

leader key is `<Space>` .

- Save and Quit

| keymap | function |
| ------ | -------- |
| `leader` + s | save |
| `leader` + q | save and quit |
| `Ctrl`+`Alt`+`shift`+ q | force quit |
| `Ctrl` + q | quit all file |
| Q | just quit |

- Copy and Paste

| keymap | function |
| ------ | -------- |
| `leader` + y | In visual mode this will copy to system clipboard |
| `leader` + p | In nomal mode this will paste system clipboard text to editor |

- Tab page

| keymap | function |
| ------ | -------- |
|   tu   | create new page |
|   tk   | move to next page |
|   th   | move to front page |

- Windows

| keymap | function |
| ------ | -------- |
| `leader` + <arrow> | Switch windows |
| `arrow` | Resize windows in normal mode |

- File Exploer

| keymap | function |
| ------ | -------- |
|   tt   | open coc explorer |
|   ?    | open coc explorer guide |
|   R    | open ranger (install ranger first) |

## Coding

- Coc.nvim

| keymap | function |
| ------ | -------- |
| `LEADER` + -/= | Move to next of front diagnostic |
| `LEADER` + d | Coc diagnostic |
| `LEADER` + h | Open definition |

- Auto align

| keymap | function |
| ------ | -------- |
| `:EasyAlign<CR>+<KEYWORD>`  | In virsual mode or normal mode this will active align plugin. |

