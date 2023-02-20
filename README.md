<h1 align="center">My Neovim Configuration</h1>
<img src="./docs/images/screenshot.png" width="45%" align="right"/>

![badge](https://github.com/avimitin/nvim/actions/workflows/lint.yml/badge.svg)
![badge](https://img.shields.io/github/license/Avimitin/nvim)

## Features

* ***Handy*** and ***Smoothy***: There will always be a panel to remind you
when you forget your key settings. There will always be a short keystroke to help
you get to the place you want to jump to. There will always…
* ***Powerful*** coding experience: With the power from nvim-lspconfig, we can
have "IDE Level" coding experience in the terminal.
* ***Fancy looking***: Talk is cheap, see the [gallery](#Gallery).
* ***Easy Customize***: Always injecting new configuration, wherever you like.

## Getting Start

This configuration is compatible with neovim 0.8+ version.

```bash
git clone --depth=1 https://github.com/Avimitin/nvim.git ~/.config/nvim
```

Finally, input `nvim` to open the editor, and all plugins will be downloaded automatically.

```bash
nvim
```

## Document

See [Docs](https://avimitin.github.io/nvim).

## Customize

See [docuement](./lua/core/README.md)

## Gallery

<details>
    <summary markdown="span">Utilities</summary>

| Easy in-file jump                             |
| --------------------------------------------- |
| ![LightSpeed](./docs/images/lightspeed.png)   |

</details>


<details>
    <summary markdown="span">Markdown Utils</summary>

| Markdown Preview                                   |
|----------------------------------------------------|
| ![image](./docs/images/neovim-md.png)              |

| Table                                              |
|----------------------------------------------------|
| ![vim-table-mode-gif](./docs/images/tablemode.gif) |

</details>


<details>
    <summary markdown="span">Coding Utils</summary>

| Diagnostic Panel |
|-----------------|
| ![image](./docs/images/trouble.png)  |

| Code Completion                       |
|---------------------------------------|
| ![coding](./docs/images/nvim-cmp.png) |

| Command Completion                                 |
|----------------------------------------------------|
| ![cmp-cmdline](./docs/images/nvim-cmp-cmdline.png) |

| Inline diagnostic analytics |
|-----------------------------|
| ![lsp-line](./docs/images/inline.png) |

| Signature Help                       |
|--------------------------------------|
| ![lsp-popup](./docs/images/help.png) |

| Code Actions                                    |
|-------------------------------------------------|
| ![lsp-codeaction](./docs/images/codeaction.png) |

| Diagnostic                                      |
|-------------------------------------------------|
| ![lsp-diagnostic](./docs/images/diagnostic.png) |

| Debug CPP                               |
|-----------------------------------------|
| ![cpp](./docs/images/dap-debug-cpp.png) |

| Debug Rust                                |
|-------------------------------------------|
| ![Rust](./docs/images/dap-debug-rust.png) |

| Code navigate                          |
|----------------------------------------|
| ![Navigate](./docs/images/def-ref.png) |

| Project grep                                        |
|-----------------------------------------------------|
| ![live-grep](./docs/images/telescope-live-grep.png) |

| Symbol search                                   |
|-------------------------------------------------|
| ![symbols](./docs/images/telescope-symbols.png) |

</details>

<details>
    <summary markdown="span">File Manage</summary>

| nvim-tree                                 |
|-------------------------------------------|
| ![nvim-tree](./docs/images/nvim-tree.png) |

| Find file                                           |
|-----------------------------------------------------|
| ![find-file](./docs/images/telescope-find-file.png) |

</details>

<details>
    <summary markdown="span">Git Helper</summary>

| Fugitive                                       |
|------------------------------------------------|
| ![fugitive](./docs/images/neovim-fugitive.png) |

</details>


<details>
    <summary markdown="span">Themes</summary>

| Kanagawa Theme                          |
|-----------------------------------------|
| ![kanagawa](./docs/images/kanagawa.png) |

| Deus Theme                           |
| ------------------------------------ |
| ![kanagawa](./docs/images/deus.png)  |

| GitHub Light Theme                                                                                                                                                        |
|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| ![gitHub](https://camo.githubusercontent.com/4bb7ad6c319b5ce63bed16cb25753e603fee510d59a1fad0245bc3d0bda8445d/68747470733a2f2f696d6775722e636f6d2f4f5077424449342e706e67)

</details>

## License

This configuration since commit `912416ae9c4b55501b23a91d774b567ba8697dd1` are
licenced under the Apache 2.0 license.

另附：禁止在 CSDN，bilibili 等国内平台使用该配置文件进行任何活动。
你只保有自己修改部分的权利。

## Development Related

### Versioning

Version will be released in `cvYYYY.0M.0D` format. I will try to release update each weak.

### Changelog

See [RELEASES.md](./RELEASES.md)

<br/>
