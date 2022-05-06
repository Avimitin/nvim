# Customize

There are some frequently change value that you don't want to be traked by git.
Like colorscheme or some other settings.
This can all be defined in a optional Lua module.

## Details

Create a file named `custom.lua` under the `~/.config/nvim/lua` directory.
This file is already added into `.gitignore` file, so any changes to it will
not be tracked.

Create a new configuration table and return it at the end.

```lua
local M = {
  theme = "kanagawa",
  has_fcitx5 = true,
}
```

## Fields

Current supported options:

| option       | meaning                                                                           |
|--------------|-----------------------------------------------------------------------------------|
| `theme`      | colorscheme, read [colors](./colors.md) for tips and tricks                       |
| `has_fcitx5` | enable this if you want to switch fcitx5 automatically when you leave insert mode |

