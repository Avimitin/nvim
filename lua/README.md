# Plugins Structure

## editor

This folder contains settings and options for neovim itself.

- `init.lua`: The entry point of the scripts.
- `autocmd.lua`: Definitions for the event driven auto commands.
- `keymap.lua`: Mappings for the editor built in commands.
- `options.lua`: Settings for the editor.
- `utils.lua`: Some function that I don't want to write twice.
You can ignore this file.

## plugins

This folders contains plugins definition and their configurations.
Modules are separated by their functionality.

- `coding/`: Language Server Protocol plugins and other coding related plugins.
- `colorscheme/`: Colorscheme plugins.
- `completion/`: Auto completion plugins. It is currently nvim-cmp and its plugins.
- `enhance/`: Some useful plugin that can enhance the neovim writing experience.
- `git/`: Git wrapper plugins.
- `lib/`: Built-in Lua library. It currently contains JSON and buffer delete API.
You can ignore this directory.
- `markdown/`: Some plugins that can enhance markdown writing experience.
- `init.lua`: Scripts that can automate plugins installation.
Also a entry point for accessing the above modules.
