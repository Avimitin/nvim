# Gopher 的 neovim 配置

## 前言

这个项目由 [theniceboy/nvim](https://github.com/theniceboy/nvim) 所启发，精简并魔改了大部分配置内容，使其更符合我的编程习惯和 qwerty 键盘的习惯。

## 使用建议

我极力推荐你 fork 一份到自己的仓库然后 clone 到自己的 `$home/.config` 目录下，这样你可以随时方便的存档和迁移，以及做出自己的修改。

## 需要安装的依赖

```bash
sudo apt install python3 python3-pip nodejs npm

#pynvim
pip3 install pynvim

#nodejs
npm install -g neovim
```

> 如果有遇到 nodejs 的 EACCES 错误，一般是不推荐直接 sudo 覆盖的（虽然我为了偷懒直接这么做）。你可以查阅 [Resolving EACCES permissions errors when installing packages globally](https://docs.npmjs.com/resolving-eacces-permissions-errors-when-installing-packages-globally) 进行官方推荐的权限问题修正。

除了上述依赖以外，我还极力推荐你安装 nerd font 以获得大量图标支持，nerd font 的[下载](https://www.nerdfonts.com/font-downloads)与[安装](https://github.com/ryanoasis/nerd-fonts/tree/master/patched-fonts/JetBrainsMono#another-ide-or-an-older-version-of-a-jetbrains-ide)。（安利一下 `Jetbrains Mono` ，是一款非常美观且非常适合编程的等宽字体）

## 安装之后

输入 `nvim` 启动 neovim，然后输入 `:checkhealth` 测试依赖齐全否，除了 Python3 和 Nodejs, 带 optional 的不一定要装。

然后进入你的家目录下的 .config 文件夹，即 `~/.config` , 把仓库 clone 下来。再次打开 neovim 会自动下载 vim-plug 依赖。等待 vim-plug 和 coc 的插件安装完，一个比 VSCode 还要强大的编辑器就在你面前啦。

> 如果遇到 `Can't not resolve host 'raw.githubusercontent.com` 之类的错误，你可以手动创建 `~/.config/nvim/autoload` 文件夹，然后手动安装 [plug.vim](https://github.com/junegunn/vim-plug#vim)。

## 按键介绍

- 指针

```text
		^
		u
< h   k >
		j
		v
```

| key | function |
| --- | -------- |
|  h  | 左移光标 |
|  k  | 右移光标 |
|  u  | 上移光标 |
|  j  | 右移光标 |
|  H  | 把光标移动到行首 |
|  K  | 把光标移动到行尾 |
|  U  | 把光标往上移动5行 |
|  J  | 把光标往下移动5行 |
|  W  | 往前移动5个字母 |
|  B  | 往后移动5个字母 |
| `<C-a>` | 在编辑模式下跳转到行尾 |

> 同时由于设置了相对行号，你在可以输入数字加 gg 回车跳转某一指定行号，亦或是输入数字 + u/j 来跳转上下相对行

- 编辑

| key | function |
| ---- | ---- |
| i | 进入编辑模式 |
| D | 删除整行 |
| X | 剪切整行 |
| `Ctrl+z` | 撤回操作 |
| `Ctrl+r` | 重做操作 |
| `Space+s` | 保存 |
| `Space+q` | 保存并退出 |
| `Q` | 什么都不做直接退出 |
| `Space+y` | 把选中的文字复制到系统剪贴板 |
| `Space+Space` | 找到下一个 `<++>` 标记并删除 |

- 窗口

| key | function |
| -------- | -------- |
| s+u/j/h/k | 把窗口往上/下/左/右分裂 |
| s+w/b | 把当前窗口垂直/平行放 |
| 箭头 | 在普通模式下调整窗口大小 |
| `<LEADER>+箭头` | 光标在窗口间切换 |

- 标签页

| key | function |
| -------- | -------- |
| tu | 创建新的标签页 |
| t+h/k | 左右切换标签页 |
| t+m+h/k | 左右移动标签页位置 |

- 其他有意思的功能

| key | function |
| -------- | -------- |
| `<Ctrl>+c` | 编辑模式下可以把光标当前行设置在中间 |
| `<LEADER>o` | 把当前光标所在位置的缩进行折叠在一起 |
| `<回车>` | 在普通模式下，输入一次回车会把被符号包围的文字选中 |
| tt | 打开文件浏览器 |

## 插件键位介绍

- coc.nvim

coc.nvim 是用来提供代码补全的插件
