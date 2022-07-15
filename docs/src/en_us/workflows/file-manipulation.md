# How to interact with projects files

There are multiple plugin that can help you find and edit a file.

## #1 Nvim-tree plugin

Nvim-tree.lua is a plugin that show files in tree view.
The configuration is located in `lua/plugins/modules/enhance/config/nvim-tree.lua`

![image](https://raw.githubusercontent.com/kyazdani42/nvim-tree.lua/master/.github/screenshot.png)

### Keymap

* Use <kbd>;t</kbd> to toggle the tree view.
* Press <kbd>R</kbd> to refresh the file tree.
* Press `?` inside the nvim-tree to get list of keymap.

## #2 Telescope Fuzzy Find

Telescope support fuzzy find file like what fzf did.

![fuzzy find](https://github.com/Avimitin/nvim/raw/master/docs/images/telescope-find-file.png)

### Keymap

* Press <kbd>;f</kbd> to open fuzzy find file.

## #3 nvim-cmp completion

nvim-cmp can fuzzy complete file path too! Press `:` into command line, then input
file path to trigger completion.

## #4 VFiler

VFiler is a full functionality file manager written in pure Lua.

![vfiler image](https://github.com/Avimitin/nvim/raw/master/docs/images/vfiler.png)

Use command `VFiler` to open the plugin.
