# 这篇文章已经严重过时了，请以英文版为主

# Gopher 的 neovim 配置

![大图](../image/screenshot.png)

## 特性

- 超快！启动仅需 `50ms` （使用命令 `nvim --staruptime`）
- 语法检查支持和自动补全支持！
- 你一定熟悉的数结构文件管理器
- IDE 都有的函数和变量名跳转
- 简单好用的缓存管理
- 高速模糊文件名查询
- 漂浮的终端窗口
- 和众多 IDE 一样的 git 信息
- 超快超顺手的文件位置跳转
...

> 48ms 指在 SSD 环境下，使用命令 `nvim --startuptime startup.log -c exit && tail -100 startup.log` 测出的最好结果。

## 参照

这个项目由 [theniceboy/nvim](https://github.com/theniceboy/nvim) 所启发
并仿照 [siduck76/NvChad](https://github.com/siduck76/NvChad) 改写为
全 Lua 脚本。

## 使用建议

我极力推荐你 fork 一份到自己的仓库然后 clone 到自己的 `$home/.config` 目录
下，这样你可以随时方便的存档和迁移，以及做出自己的修改。

```bash
# linux or mac user
git clone https://github.com/avimitin/nvim ~/.config

# neovim 第一次启动可能会遇到插件加载不完全的问题
# 所以我写了脚本来自动退出
nvim -c 'autocmd User PackerComplete quitall'
```

## 需要安装的依赖

因为使用了完全的 Lua 脚本，Vim 和 Neovim 的 0.4 稳定版本都是不支持的，
你需要自己本地编译一份最新的 git 版本。

```bash
# 如果你已经安装了，先卸载
sudo apt remove neovim 

sudo add-apt-repository ppa:neovim-ppa/unstable
sudo apt-get update
sudo apt install neovim

# Arch 用户只需要下载 nightly-bin
yay -S neovim-git
```

除了上述依赖以外，我还极力推荐你安装 nerd font 以获得大量图标支持，nerd font 的
[下载](https://www.nerdfonts.com/font-downloads)与
（安利一下 `Jetbrains Mono` ，是一款非常美观且非常适合编程的等宽字体）

## 按键介绍

### 指针

```text
    ^
    j
< h   l >
    k
    v
```

| key     | function               |
| ---     | --------               |
| h       | 左移光标               |
| l       | 右移光标               |
| k       | 上移光标               |
| j       | 下移光标               |
| H       | 把光标移动到行首       |
| K       | 把光标移动到行尾       |
| K       | 往上滚动页面           |
| J       | 往上滚动页面           |
| W       | 往前移动5个字母        |
| B       | 往后移动5个字母        |
| jj      | 退出编辑模式           |
| `<C-a>` | 在编辑模式下跳转到行尾 |

### 编辑

| key           | function                     |
| ----          | ----                         |
| D             | 删除整行                     |
| X             | 剪切整行                     |
| `Ctrl+z`      | 撤回操作                     |
| `Ctrl+r`      | 重做操作                     |
| `Space+s`     | 保存                         |
| `Space+q`     | 保存并退出                   |
| `<Ctrl+Q>`    | 什么都不做直接退出           |
| `Space+y`     | 把选中的文字复制到系统剪贴板 |
| `Space+p`     | 从系统剪贴板粘贴文字         |

### 窗口

| key       | function                 |
| --------  | --------                 |
| s+u/j/h/k | 在窗口之间跳转光标       |
| s+r+k/h   | 旋转窗口                 |
| 箭头      | 在普通模式下调整窗口大小 |

### 标签页

| keymap      | function                           |
| ------      | --------                           |
| `,`         | 切换到前一个标签页                 |
| `.`         | 切换到后一个标签页                 |
| `<Alt> + >` | 把当前标签页向前移动               |
| `<Alt> + <` | 把当前标签页向后移动               |
| `<Alt> + x` | 开启选中模式，并关闭你选中的选项卡 |

### 其他功能

| key         | function                                           |
| --------    | --------                                           |
| `<Ctrl>+c`  | 编辑模式下可以把光标当前行设置在中间               |
| 空格+o      | 打开文件管理器     |
| `<回车>`    | 在普通模式下，输入一次回车会把被符号包围的文字选中 |
| `<Ctrl>+\`  | 打开终端                                           |

## 插件键位介绍

### 语法检查和自动补全

我的键位设置:

| keymap       | function           |
| ------       | --------           |
| `[d/]d`      | 在错误段落之间跳转 |
| `LEADER` + e | 显示所有错误       |
| `LEADER + h` | 显示帮助信息       |
| gd           | 跳转到定义位置     |
| gD           | 跳转到引用位置     |

### hop 

可以展开函数和变量的定义列表以及他们的位置，对于超级大的项目来说，可以方便的在文件内各个方法间跳转

| key       | function           |
| --------- | ------------------ |
| space + l | 打开 hop         |
| 回车      | 跳转到函数定义位置 |

### Telescope

提供文件预览和跳转，支持全文搜索。

| key        | function               |
| ---------- | ---------------------- |
| 空格+ff    | 启动文件名搜索         |
| 空格+fg    | 启动全文搜索           |
| Ctrl + t   | 在新标签页打开文件     |

### GitSign

可以在行号附近显示当前行的 diff 的信息，以及简单的 git 操作

| key              | function                       |
| ---------------- | ------------------------------ |
| g + i + u        | 撤销暂存                       |
| g + i + s        | 暂存修改                       |
| g + i + p        | 查看差异                       |
| g + i + r        | 清除改动                       |
| g + i + n        | 跳转上一个修改                 |
| g + i + m        | 跳转下一个修改                 |

### far.vim

[far.vim](https://github.com/brooth/far.vim) 可以在当前目录内的所有文件中搜索给定关键词，以及批量替换

![](https://cloud.githubusercontent.com/assets/9823254/20861878/77dd1882-b9b4-11e6-9b48-8bc60f3d7ec0.gif)

| key        | function                                  |
| ---------- | ----------------------------------------- |
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

### vim-go

在 go 文件中使用 `go/r/t` 来触发自动导入，自动运行和自动测试

### lazygit

<img style="max-width: 500px; max-height: 500px" 
src="https://raw.githubusercontent.com/jesseduffield/lazygit/assets/rebase.gif" />

使用 `<CTRL>+g` 打开 lazygit

## Markdown

### 预览

输入 `:MarkdownPreview` 来激活预览, 你可能需要在 `core/plugins.vim` 下
找到 markdown preview 并修改默认浏览器和端口。

### Toc

输入 `:GenTocGFM` 可以在当前行生成文章目录。

### Table

选中你的表格, 输入 `:EasyAlign *|` 可以把你的表格对齐。

