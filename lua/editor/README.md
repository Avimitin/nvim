# Docs

This folder contains some basic setup for neovim itself.

## init.lua

This file is the entry point of the whole configuration. It will initialize default configuration
for this configuration, read and parse the custom.lua file, and finally load all the plugins.

By default, this configuration will automatically install `lua`, `vim`, `rust` grammar highlight.
It will use `kanagawa` as default colorscheme. The markdown previewer will be `firefox`, and all
my custom auto command will be disabled. You can change this settings in `custom.lua` file.

## keymap.lua

This file contains key remapping. Like `K` will not open document but jump 5 lines up.
If you wants default key mapping, feel free to delete them here.

## options.lua

This file set up some useful option for neovim, for example, enable the number column.
I've added comments for the effect of those options, if you got some unexpected behavior, feel
free to close them.

## utils.lua

This file is not important, just some function that I don't want to write twice.
