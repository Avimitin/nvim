# Packer

Packer is a plugin manager. It will download plugins from GitHub
and add plugin into runtime path automatically. What it differ to other
is that it can lazyload plugins easily.

Here I will describe some usage and give some useful tips.
For more details please read the packer.nvim README file:
https://github.com/wbthomason/packer.nvim

## Basic

The packer plugin is initialized in the `lua/plugins.lua` file.

I called the `require('packer').init` in the beginning, and put two settings
into it.

The display setting set a floatting window with single line border for the
packer panel.
You can see the visual effect when you call any packer command.

The git setting sets a timeout which will abort the update process when the git
clone process is over 1 minute.

And the `require('packer').startup` function at the end will handle all the
plugins.

## Commands

* `:PackerInstall`: Run install process when you import new plugin.
* `:PackerClean`: Remove local plugins which is not in the plugin list.
* `:PackerUpdate`: Update all the plugin in the plugin list.
* `:PackerSync`: Run all the above command in one.

## Add new plugin

If you want to add a plugin from GitHub, you can use the `use` function
to add the plugin.

For example, if you want to add the plugin `trouble.nvim`, and it's URL is
https://github.com/folke/trouble.nvim, you only need to add the below snippet
inside the `require('packer').startup` function:

```lua
use {
  'folke/trouble.nvim'
}
```

After saving the file, run command `:PackerInstall` to install the plugin.

## Delete plugins

You just need to delete the declaration line, then run `:PackerClean`.

## Useful fields

This is the key to make your neovim faster.

| fields    | value                      | usage                                                                |
|-----------|----------------------------|----------------------------------------------------------------------|
| disable   | boolean                    | Mark a plugin as inactive                                            |
| as        | string                     | Specifies an alias under which to install the plugin                 |
| installer | function                   | Specifies custom installer. See "custom installers" below.           |
| updater   | function                   | Specifies custom updater. See "custom installers" below.             |
| after     | string or list             | Specifies plugins to load before this plugin. See "sequencing" below |
| rtp       | string                     | Specifies a subdirectory of the plugin to add to runtimepath.        |
| opt       | boolean                    | Manually marks a plugin as optional.                                 |
| branch    | string                     | Specifies a git branch to use                                        |
| tag       | string                     | Specifies a git tag to use                                           |
| commit    | string                     | Specifies a git commit to use                                        |
| lock      | boolean                    | Skip updating this plugin in updates/syncs. Still cleans.            |
| run       | string, function, or table | Post-update/install hook. See "update/install hooks".                |
| requires  | string or list             | Specifies plugin dependencies. See "dependencies".                   |
| rocks     | string or list             | Specifies Luarocks dependencies for the plugin                       |
| config    | string or function         | Specifies code to run after this plugin is loaded.                   |

## Lazyload settings

| fields         | value              | usage                                                                                                                                          |
|----------------|--------------------|------------------------------------------------------------------------------------------------------------------------------------------------|
| setup          | string or function | pecifies code to run before this plugin is loaded.                                                                                             |
| cmd            | string or list     | Specifies commands which load this plgin. Can be an autocmd pattern.                                                                           |
| ft             | string or list     | Specifies filetypes which load this plugin.                                                                                                    |
| key            | string or list     | Specifies maps which load this plugin. See "Keybindings".                                                                                      |
| event          | string or list     | Specifies autocommand events which load this plugin.                                                                                           |
| fn             | string or list     | Specifies functions which load this plugin.                                                                                                    |
| cond           | str/func/list of str/func   | Specifies a conditional test to load this plugin                                                                                      |
| module         | string or list     | Specifies Lua module names for require. When requiring a string which starts with one of these module names, the plugin will be loaded.        |
| module_pattern | string/list        | Specifies Lua pattern of Lua module names for require. When requiring a string which matches one of these patterns, the plugin will be loaded. |

## FAQ

### How to set up options **before** plugin load?

Use the setup field. Example config:

```lua
use {
  'path/to/plugin',
  setup = function()
    -- this function will be called before the plugin loaded
    vim.g.settings = "your settings"
  end
}
```

### How to setup config **after** plugin load?

Use the config field. Example config:

```lua
use {
  'plugin',
  config = function()
    require('plugin').setup{}
  end
}
```

### How to load plugin only when you called it's command

For example, the vim-fugitive provide command `:Git`, you can load the
vim-fugitive command only when you call the Git command:

```lua
use {
  'tpope/vim-fugitive',
  cmd = {
    'G', 'Git',
    'Ggrep',
    'Gdiffsplit',
    'GBrowse'
  }
}
```

With the above settings, vim-fugitive will not be sourced when you open the
editor. It will only be sourced after you called the
`:G/:Git/:Ggrep/:Gdiffsplit/:GBrowse` command.

### How to load plugin only in specific filetype

In the below example, nvim-lsp-installer will only be loaded when you open file
with `.json`, `.py`, and `.js` extension.

```lua
use {
  'williamboman/nvim-lsp-installer',
  ft = { "json", "python", "javascript" },
  config = function()
    require("lspconfig")
  end
}
```

> **Important notes**:
>
> The ft field has more priviledge than the after field.
> So if you have `ft` and `after` for same plugin, it will first evaluate the
> filetype not the after field.

### How to load plugin for specific module call

The telescope.nvim here will only be loaded after any code call the
`require("telescope")`.

```lua
use {
  'nvim-telescope/telescope.nvim',
  requires = {
    'nvim-lua/popup.nvim',
    'nvim-lua/plenary.nvim'
  },
  config = function()
    require("config.telescope_config")
  end,
  module = 'telescope'
}
```
