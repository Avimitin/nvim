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

See [document](./lua/core/README.md)

## Treesitter

To make treesitter compatible with the stable neovim, and to make the share library compilation process reproducible and clean,
this configuration uses nix package manager to manage the treesitter parser plugin.
The flake output provides package `treesitter-parsers` to modify the neovim runtime path to point to this parser plugin.
To use it, you can use home-manager to help you put this package into neovim's data directory.

- Example home-manager configuration:

```nix
# This can help auto load
xdg.dataFile = {
    nvim-treesitter-parsers = {
        source = nvim-flake.packages."x86_64-linux".treesitter-parsers;
        target = "nvim/siter/plugin/treesitter-parsers.lua";
    };
}
```

You can also filter parsers by their name: here `parsers` is a callable attribute, and invoke it with an array can filter data inside it.

```nix
xdg.dataFile = {
    nvim-treesitter-parsers = {
      source = let
        parsers = pkgs.callPackage ./nix/treesitter-parsers.nix {};
        toNvimPlug = pkgs.callPackage ./nix/set-rtp.nix {};
      in
        # And now, only "bash" and "lua" plugin will be prepended into neovim runtime path
        toNvimPlug "treesitter-parsers" (parsers [ "bash" "lua" ]);
      target = "nvim/site/plugin/treesitter-parsers.lua";
    };
}
```

To add more language parser, you can attach more parser at the end of the [nix file](./nix/treesitter-parsers.nix).
The array expect the argument in this form: `[{ name: xxx; hash: xxx; }, ...]`,
where:

  - name string: The name of the language
  - hash string: The input hash, you can leave it blank and wait for nix hash report the correct hash
  - needs_generate bool: When true, tree-sitter CLI will be used to generate the parser.
  - srcRoot string: Specify where the parser source located. Some repository will vendor two or more parser source code in one repository.

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

See [CHANGELOG.md](./CHANGELOG.md)

<br/>
