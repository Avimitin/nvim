<h1 align="center">My Neovim Configuration</h1>
<img src="./docs/images/screenshot.png" width="45%" align="right"/>

![badge](https://img.shields.io/github/license/Avimitin/nvim)

## Features

* ***Handy*** and ***Smoothy***: There will always be a panel to remind you
when you forget your key settings. There will always be a short keystroke to help
you get to the place you want to jump to. There will always…
* ***Powerful*** coding experience: nvim-lspconfig powered great LSP experient.
* ***Fancy looking***: Well designed, denoised, uncluttered UI. Talk is cheap, see the [gallery](#Gallery).

## Getting Start

This configuration is compatible with neovim 0.8+ version.

```bash
git clone --depth=1 https://github.com/Avimitin/nvim.git ~/.config/nvim
```

Finally, input `nvim` to open the editor, and all plugins will be downloaded automatically.

```bash
nvim
```

To use this in your home-manager, you can use the xdg.configFile attribute:

```nix
{ pkgs }:
{
    xdg.configFile = {
        neovim = {
            target = "nvim";
            source = pkgs.fetchFromGitHub {
                repo = "nvim";
                owner = "Avimitin";
                rev = "...";
                hash = "...";
            }
        };
    };
}
```

## Project Structure


- `lua`: the configuration core

    * `key-mapping.lua`: my modification to the built-in key mappings
    * `pack.lua`: script to download lazy.nvim plugin manager
    * `core/`: my modification to the built-in options and auto commands
    * `completion/`: plugins and configuration for LSP and vim command completion
    * `git/`: plugin and configuration for using git in Neovim
    * `lang/`: plugins and configuration for using LSP server and get diagnostic in Neovim
    * `libs/`: functions that I don't want to write twice
    * `note/`: markdown and neorg support for neovim
    * `tools/`: miscellaneous plugins that can enhance editing experience
    * `treesitter/`: plugins for text object highlight and editing
    * `ui/`: plugins for decorating the neovim

- `after/ftplugin/<lang>.lua`: configured LSP settings for each different language
- `ftdetect`: list of script to help neovim identify filetype for some file
- `indent`: list of script to help neovim properly set indentation
- `syntax`: additional syntax detection for some file type
- `vsnip`: my snippets


## Treesitter parsers in nix

> Ignore this if you are not a nix user

To make treesitter compatible with the stable neovim, and to make the share library compilation process reproducible and clean,
this configuration provides a treesitter parser nix expression to manage the treesitter parser plugin.
The flake output an overlay providing package `nvim-treesitter-parsers`.
To use it, you can use home-manager to help you put this package into neovim's data directory.

- Example flake based home-manager configuration:

```nix
# flake.nix

{
  description = "Flakes to reference treesiter";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nvim = {
      url = "github:Avimitin/nvim";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, home-manager, nvim }:
    let
      overlays = [ nvim.overlays.default ];
      pkgsIn = import nixpkgs { system = "x86_64-linux"; inherit overlays;  };
    in
    {
      homeConfigurations = {
        "laptop" = home-manager.lib.homeManagerConfiguration {
            inherit pkgs;
            modules = [ ./laptop.nix ];
        };
      };
    };
}
```

And in the example `laptop.nix`, you can create a file in `$XDG_DATA_HOME` to let neovim automatically load those parsers:

```nix
{ pkgs, ... }:
{
    xdg.dataFile.generate-nvim-treesitter-parsers = let
        tsLoader = pkgs.generate-nvim-treesitter-parsers [
          { name = "bash"; hash = "sha256-b1r/T+Y4Kmui/pHsncozP8OO6rMMHJj+Xaa2Qzwo/cI="; }
          { name = "c"; hash = "sha256-sB8fNfusjC9yTlrizb2mufDzQPvBajTJC+ewF9awBqA="; }
          { name = "cpp"; hash = "sha256-27QjVy8quWyGhFCv/6GATG1xjGnkB9LTcvlPMuR3NB0="; }
        ];
      in
      {
        source = "${tsLoader.passthru.loaderScript}";
        target = "nvim/site/plugin/treesitter-parsers.lua";
      };
}
```

The array expect the argument in this form: `[{ name: xxx; hash: xxx; }, ...]`,
where:

  - name string: The name of the language
  - hash string: The input hash, you can leave it blank and wait for nix hash report the correct hash
  - needs_generate bool: When true, tree-sitter CLI will be used to generate the parser.
  - srcRoot string: Specify where the parser source located. Some repository will vendor two or more parser source code in one repository.

See [my overlay](./overlay.nix) for detail examples and the current available parsers.

Also you might found update the parser hash one by one really annoying, so the `generate-nvim-treesitter-parsers` also contains a update script.
To use it, first you will need to place the script file somewhere:

```nix
xdg.dataFile =
let
  tsLoader = pkgs.generate-nvim-treesitter-parsers [
    { name = "bash"; hash = "sha256-b1r/T+Y4Kmui/pHsncozP8OO6rMMHJj+Xaa2Qzwo/cI="; }
    { name = "c"; hash = "sha256-sB8fNfusjC9yTlrizb2mufDzQPvBajTJC+ewF9awBqA="; }
    { name = "cpp"; hash = "sha256-27QjVy8quWyGhFCv/6GATG1xjGnkB9LTcvlPMuR3NB0="; }
    { name = "yaml"; hash = "sha256-RrYFKrhqFLsjQG+7XFbcQ2eYy2eyig5/r+MYO8DId4g="; }
  ];
in
{
  nvim-treesitter-parsers = {
    source = "${tsLoader}${tsLoader.passthru.luaPath}";
    target = "nvim/site/plugin/treesitter-parsers.lua";
  };
  nvim-treesitter-updater = {
    source = "${tsLoader.passthru.updateScript}/bin/treesitter-hash-batch-updater";
    target = "nvim/assets/treesitter-updater.bash"; # or any other path you like
  };
};
```

This script accept single argument, which should point to the hash definition file.
In our example above, it is the 'laptop.nix' file, so run the bash script `~/.local/share/nvim/assets/treesitter-updater.bash laptop.nix`,
and wait for it finish its job.

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

| LSP Progress |
|-----------------|
| ![image](./docs/images/lsp-progress.png)  |

| Symbol Tree |
|-----------------|
| ![image](./docs/images/symboltree.png)  |

| Code Completion                       |
|---------------------------------------|
| ![coding](./docs/images/nvim-cmp.png) |

| Command Completion                                 |
|----------------------------------------------------|
| ![cmp-cmdline](./docs/images/commandline-completion.png) |

| Errorlens Like diagnostic |
|-----------------------------|
| ![lsp-line](./docs/images/errorlens.png) |

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
    <summary markdown="span">File Manager</summary>

| neotree                                 |
|-------------------------------------------|
| ![nvim-tree](./docs/images/neotree.png) |

| Find file                                           |
|-----------------------------------------------------|
| ![find-file](./docs/images/telescope-find-file.png) |

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
