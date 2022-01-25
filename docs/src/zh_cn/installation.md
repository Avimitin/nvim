# 先决条件

* **Neovim** (必要)

目前我使用 `v0.7.0-dev+920-g7e2ce35e3`。 如果你遇到了任何错误，请检查你的版本。

你可以按照
[neovim 安装方式](https://github.com/neovim/neovim/wiki/Installing-Neovim)
构建最新版本的 neovim。

如果你是 ArchLinux 用户，你可以运行: `yay -S neovim-git`

* **Nerdfont** (必要)

我的大部分设置都是基于 nerd font 的。强烈建议你安装 [nerdfont](https://www.nerdfonts.com/font-downloads) 来取得很棒的图标支持。

* **ripgrep** (必要)

我使用 ripgrep 作为插件 telescope, nvim-cmp, anyjump 的搜索程序。ripgrep 是一个性能更好的grep 替代。

在此查看更多有关信息 [readme](https://github.com/BurntSushi/ripgrep)

* *Surf* (可选)

我使用 [Surf](https://surf.suckless.org/) 作为 Markdown 预览器。用 Firefox 预览 markdown 太重了。如果你有兴趣，可以按照官方说明来更改。没有的话，你也可以使用下列命令简单的替换。

```bash
sed -i 's/surf/firefox/g' ./lua/config/mkdp.lua
```

> * 如何构建surf
>
> ```bash
> # Arch linux 包含了许多有用的库
> # 其他发行版需要自己去查看文档
> git clone https://git.suckless.org/surf
> cd surf
> sudo make clean install
> ```

# 安装

```bash
# 目前没有 Windows 支持
git clone https://github.com/YOUR_USER_NAME/nvim ~/.config/nvim
```

使用`nvim`命令打开你的 neovim，等待所有插件安装完成。当安装完成后，请退出并重新打开以加载所有的插件。

如果 neovim 没有自动安装插件，请使用命令`:PackerSync`来手动安装这些插件。并请打开一个 issue 来通知我这个错误。

**NOTE:** Markdown 预览插件已在另一个线程中安装，请等待安装成功的消息。否则插件无法激活。

# 清理

你需要清理下面的目录以进行新的安装。

```bash
# 插件目录
rm -rf ~/.local/share/nvim

# neovim 缓存目录
rm -r ~/.cache/nvim

# 重置插件加载顺序
rm -r ~/.config/nvim/plugin
```

# Check health

打开你的neovim，并输入以下命令，检查依赖是否已经全部安装。

```text
:checkhealth
```

### Docker

只是想尝试一下，但不想把你的本地环境搞得一团糟？我为你准备了docker脚本!

```bash
docker run -w /root -it --rm alpine:edge sh -uelic '
      apk add git neovim ripgrep alpine-sdk --update
      git clone 'https://github.com/Avimitin/nvim' ~/.config/nvim
      nvim -c "autocmd User PackerComplete quitall"
      nvim /root/.config/nvim/README.md
  '
```
