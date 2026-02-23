build:
  @nvim -u NONE -l dump_plugins.lua
  @nom build '.#neovim'
