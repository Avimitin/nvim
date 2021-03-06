# Gopher 的 neovim 配置

![大图](https://cdn.jsdelivr.net/gh/Avimitin/PicStorage/pic/20210228180603.png)

## 目录

<details>
<summary>点击查看</summary>
<!-- vim-markdown-toc GFM -->

* [前言](#前言)
* [使用建议](#使用建议)
* [需要安装的依赖](#需要安装的依赖)
* [安装之后](#安装之后)
* [按键介绍](#按键介绍)
	* [指针](#指针)
	* [编辑](#编辑)
	* [窗口](#窗口)
	* [标签页](#标签页)
	* [其他功能](#其他功能)
* [插件键位介绍](#插件键位介绍)
	* [coc.nvim](#cocnvim)
	* [vista.vim](#vistavim)
	* [GitGutter](#gitgutter)
	* [far.vim](#farvim)
	* [Vim-easy-align](#vim-easy-align)
	* [wildfire](#wildfire)
	* [vim-surround](#vim-surround)
	* [Auto-pairs](#auto-pairs)
	* [vim-go](#vim-go)
	* [lazygit](#lazygit)
* [Markdown](#markdown)
	* [键位设置](#键位设置)
	* [预览](#预览)
	* [Toc](#toc)
	* [Table](#table)

<!-- vim-markdown-toc -->
</details>

## 前言

这个项目由 [theniceboy/nvim](https://github.com/theniceboy/nvim) 所启发，
精简并魔改了大部分配置内容，使其更符合我的编程习惯和 qwerty 键盘的习惯。

## 使用建议

我极力推荐你 fork 一份到自己的仓库然后 clone 到自己的 `$home/.config` 目录
下，这样你可以随时方便的存档和迁移，以及做出自己的修改。

目前为了缩小脚本大小，以及提高维护效率，我选择维护两个分支，你可以选择你的
对应机型来 clone.

```bash
# linux or mac user
git clone -b linux https://github.com/avimitin/nvim

# windows user
git clone -b windows https://github.com/avimitin/nvim
```

## 需要安装的依赖

首先极力建议你安装 neovim 0.5+ 版本来获得特性支持: 因为 0.5 版本有很多插件需要
的特性。因此非常推荐 Ubuntu 用户增加 unstable 源。

```bash
# 如果你已经安装了，先卸载
sudo apt remove neovim 

sudo add-apt-repository ppa:neovim-ppa/unstable
sudo apt-get update
sudo apt install neovim

# Arch 用户只需要下载 nightly-bin
yay -S neovim-nightly-bin
```

你需要安装 
[Python3](https://www.python.org/downloads/)
| 
[NodeJS](https://nodejs.org/en/download/) ,
关于 nodejs 的安装可以参考这篇 [文章](nodejs_install.md)

> Windows 用户需要先进入 Python3 的安装目录，复制一份 python.exe，然后改名成 python3.exe。

安装好 nodejs 和 python3 之后需要执行下面的命令来安装依赖。

```bash
#pynvim
pip3 install pynvim

#nodejs
npm install -g neovim
```

> 如果有遇到 nodejs 的 EACCES 错误，一般是不推荐直接 sudo 覆盖的（虽然我为了偷懒直接这么做）。你可以查阅 
[Resolving EACCES permissions errors when installing packages globally](https://docs.npmjs.com/resolving-eacces-permissions-errors-when-installing-packages-globally) 
进行官方推荐的权限问题修正。

除了上述依赖以外，我还极力推荐你安装 nerd font 以获得大量图标支持，nerd font 的
[下载](https://www.nerdfonts.com/font-downloads)与
[安装](https://github.com/ryanoasis/nerd-fonts/tree/master/patched-fonts/JetBrainsMono#another-ide-or-an-older-version-of-a-jetbrains-ide)。
（安利一下 `Jetbrains Mono` ，是一款非常美观且非常适合编程的等宽字体）

## 安装之后

输入 `nvim` 启动 neovim，然后输入 `:checkhealth` 测试依赖齐全否，除了 Python3 和 Nodejs, 
带 optional 的不一定要装。

然后进入你的家目录下的 .config 文件夹，即 `~/.config`,
(Windows 用户在 `C:\Users\yourname\AppData\Local`)
把仓库 clone 下来。再次打开 neovim，输入 `:PluginInstall` 
会自动下载 vim-plug 依赖。等待 vim-plug 和 coc 的插件安装完，一个比 VSCode 还要强大的编辑器就在你面前啦。

> 如果遇到 `Can't not resolve host 'raw.githubusercontent.com` 之类的错误，
你可以手动创建 `~/.config/nvim/autoload` 文件夹，然后手动安装 
[plug.vim](https://github.com/junegunn/vim-plug#vim)。
不过连不上 Github 后面的插件也不好安装，建议配好代理吧。

## 按键介绍

### 指针

```text
    ^
    u
< h   k >
    j
    v
```

| key     | function               |
| ---     | --------               |
| h       | 左移光标               |
| k       | 右移光标               |
| u       | 上移光标               |
| j       | 下移光标               |
| H       | 把光标移动到行首       |
| K       | 把光标移动到行尾       |
| U       | 把光标往上移动5行      |
| J       | 把光标往下移动5行      |
| W       | 往前移动5个字母        |
| B       | 往后移动5个字母        |
| l       | 向前移动一个词         |
| L       | 向后移动一个词         |
| jj      | 退出编辑模式           |
| `<C-a>` | 在编辑模式下跳转到行尾 |

> 同时由于设置了相对行号，你在可以输入数字加 gg 回车跳转某一指定行号，亦或是输入数字 + u/j 来跳转上下相对行

### 编辑

| key           | function                     |
| ----          | ----                         |
| i             | 进入编辑模式                 |
| a             | 在当前光标所在字符后开始编辑 |
| D             | 删除整行                     |
| X             | 剪切整行                     |
| `Ctrl+z`      | 撤回操作                     |
| `Ctrl+r`      | 重做操作                     |
| `Space+s`     | 保存                         |
| `Space+q`     | 保存并退出                   |
| `Q`           | 什么都不做直接退出           |
| `Space+y`     | 把选中的文字复制到系统剪贴板 |
| `Space+p`     | 从系统剪贴板粘贴文字         |
| `Space+Space` | 找到下一个 `<++>` 标记并删除 |

### 窗口

| key       | function                 |
| --------  | --------                 |
| s+u/j/h/k | 在窗口之间跳转光标       |
| s+w/b     | 把当前窗口垂直/平行放    |
| s+r+k/h   | 旋转窗口                 |
| 箭头      | 在普通模式下调整窗口大小 |

### 标签页

| key      | function           |
| -------- | --------           |
| tu       | 创建新的标签页     |
| t+h/k    | 左右切换标签页     |
| t+m+h/k  | 左右移动标签页位置 |

### 其他功能

| key         | function                                           |
| --------    | --------                                           |
| `<Ctrl>+c`  | 编辑模式下可以把光标当前行设置在中间               |
| 空格+o      | 普通模式下把当前光标所在位置的缩进行折叠在一起     |
| `<回车>`    | 在普通模式下，输入一次回车会把被符号包围的文字选中 |
| `<Ctrl>+\`  | 打开终端                                           |
| `<Space>+n` | 打开 init.vim 配置文件                             |
| `<Space>+e` | 打开新标签页并输入文件名来编辑新文件               |

## 插件键位介绍

### coc.nvim

[coc.nvim](https://github.com/neoclide/coc.nvim) 是用来提供代码补全的插件，通过配置 
[LSP(Language Server Protocol)](https://microsoft.github.io/language-server-protocol/) 
来提供语法补全和各类 IDE 的功能。

![VSCode 的补全也是依赖于 LSP 的](https://microsoft.github.io/language-server-protocol/img/vscode-css-code-complete.png)

Coc 我只设置了 Golang 和 C/C++ 的代码补全，别的语言你可以参考 
[Language Servers](https://github.com/neoclide/coc.nvim/wiki/Language-servers) 
配置语言服务器，或者搜索插件仓库 `:CocList extensions` 查看你需要的插件，
然后 `:CocInstall <PluginName>` 你想要的语法补全插件。

我的键位设置:

| keymap        | function                                 |
| ------------- | ---------------------------------------- |
| Ctrl + Space  | 在编辑模式下，刷新 coc 的缓存            |
| Space + h     | 在普通模式下，展开光标下的代码的定义信息 |
| Space + -/=   | 在普通模式下，在文件内的错误中跳转       |
| Ctrl + c      | 展开 coc 的命令                          |
| Space + r + n | 重命名变量                               |
| g + d/y/r     | 跳转到函数变量 定义/类型定义/指向        |
| tt            | 打开 coc 文件浏览                        |
| ts            | 翻译当前光标下的单词                     |

### vista.vim

[vista.vim](https://github.com/liuchengxu/vista.vim) 
可以展开函数和变量的定义列表以及他们的位置，对于超级大的项目来说，可以方便的在文件内各个方法间跳转

| key       | function           |
| --------- | ------------------ |
| space + v | 打开 vista         |
| 回车      | 跳转到函数定义位置 |

 ### LeaderF

[LeaderF](https://github.com/Yggdroot/LeaderF) 支持文件模糊搜索并启动

![](https://raw.githubusercontent.com/Yggdroot/Images/master/leaderf/leaderf_popup.gif)

| key        | function               |
| ---------- | ---------------------- |
| 空格+f     | 启动 leaderF           |
| tab        | 切换输入模式和普通模式 |
| Ctrl + u/j | 输入模式下上下移动光标 |
| Ctrl + ]   | 分裂窗口打开文件       |
| 回车       | 在当前窗口打开文件     |
| Ctrl + t   | 在新标签页打开文件     |

### GitGutter

[GitGutter](https://github.com/airblade/vim-gitgutter) 可以在行号附近显示当前行的 diff 的信息。

| key              | function                       |
| ---------------- | ------------------------------ |
| 空格 + g + u     | 恢复当前修改为上次 commit 状态 |
| 空格 + g + s     | 将当前修改加进暂存区           |
| 空格 + g + p     | 预览所有修改                   |
| 空格 + g + `-/=` | 在各个修改间跳转               |

### far.vim

[far.vim](https://github.com/brooth/far.vim) 可以在当前目录内的所有文件中搜索给定关键词，以及批量替换

![](https://cloud.githubusercontent.com/assets/9823254/20861878/77dd1882-b9b4-11e6-9b48-8bc60f3d7ec0.gif)

| key        | function                                  |
| ---------- | ----------------------------------------- |
| ctrl + f   | 在普通模式下，启动 far.vim 搜索           |
| `:Far`     | 启动搜索和替换                            |
| `:Fardo`   | 执行替换                                  |
| `:Farundo` | 撤销上次替换                              |
| x          | 在打开的 preview 中，排除掉不需要的替换项 |
| i          | 在打开的 preview 中，选中掉需要的替换项   |

### Vim-easy-align

<img style="max-width: 500px; max-height: 500px" 
src="https://raw.githubusercontent.com/junegunn/i/master/easy-align/equals.gif" />

[vim-easy-align](https://github.com/junegunn/vim-easy-align) 可以自动对齐给定的字符，
先选定一个段落，输入 `:EasyAlign` 回车，然后输入你需要作为参照物的对齐字符。

![](https://raw.githubusercontent.com/junegunn/i/master/easy-align/equals.gif)

### wildfire

[wildfire](https://github.com/gcmt/wildfire.vim) 可以帮你快速选中符号中的文字，
只需要在普通模式下输入回车。

![](https://raw.githubusercontent.com/gcmt/wildfire.vim/master/_assets/preview.gif)

### vim-surround

[vim-surround](https://github.com/tpope/vim-surround) 可以帮你快速的在选定文字周围补上给定字符。

<img style="max-width: 500px; max-height: 500px" 
src="https://camo.githubusercontent.com/334f5a06cbee4141889dfdf18a7c51a0ea408edb4d79f4dbe77e4d8b937d5a0b/68747470733a2f2f74776f2d77726f6e67732e636f6d2f696d6167652f737572726f756e645f76696d2e676966" />

| key  | function                                                                                                       |
| ---- | ------------------------------------------------------------                                                   |
| S    | 选中文字之后，输入大写S，光标消失后，输入字符就可以在选中的文字的两遍包围上你给定的字符。                      |
| CS   | 在普通模式下（不需要选中文字），把光标下的文字附近的符号替换。比如 `"Hello"`，输入 `CS"'` 把双引号换成单引号， |

> 如果是用括号包围，输入右边的括号来智能包围，比如 "hello"，输入 cs"} 就会变成 {hello}

### Auto-pairs

[auto-pairs](https://github.com/jiangmiao/auto-pairs) 是一个自动帮你补全括号和跳转括号的插件，
默认不需要设置，不过偶尔会遇到不想要的跳转情况：

```text
// 输入快捷键前
{
{
}
```

在上述情况，你可能想输入一个 `}` 来把括号补上，但是 auto-pairs 的跳转功能会直接帮你跳转到下一
行的大括号而不是插入新的括号，在这种情况下，输入 `<Ctrl>+b` 就会跳回去并帮你把这个大括号补好：


```text
// 输入快捷键后
{
{}
}
```

### vim-go

在 go 文件中使用 `gi/r/t` 来触发自动导入，自动运行和自动测试

### lazygit

<img style="max-width: 500px; max-height: 500px" 
src="https://raw.githubusercontent.com/jesseduffield/lazygit/assets/rebase.gif" />

使用 `<CTRL>+g` 打开 lazygit

## Markdown

### 键位设置

| Shortcut   | What it creates       |
| ---------- | --------------------- |
| `,n`       | ---                   |
| `,b`       | **粗体**              |
| `,s`       | ~~划掉~~              |
| `,i`       | *斜体*                |
| `,d`       | `代码`                |
| `,c`       | `代码框`              |
| `,m`       | - [ ] 勾选框          |
| `,p`       | 图片                  |
| `,a`       | [超链接]()            |
| `,1`       | # H1                  |
| `,2`       | ## H2                 |
| `,3`       | ### H3                |
| `,4`       | #### H4               |
| `,l`       | --------              |

`,f` 跳转到下一个标签并编辑 `<++>`

`,w` 跳转到下一个标签 `<++>` 并换行

### 预览

输入 `:MarkdownPreview` 来激活预览, 你可能需要在 `core/plugins.vim` 下
找到 markdown preview 并修改默认浏览器和端口。

### Toc

输入 `:GenTocGFM` 可以在当前行生成文章目录。

### Table

选中你的表格, 输入 `:EasyAlign *|` 可以把你的表格对齐。

