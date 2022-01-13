# Colors

This section describe how I manage the colorscheme.

## Config path

```text
lua/colors.lua
```

## Change theme

You can modify the variable `theme` in `colors.lua` like:

```lua
local theme = "ayu" -- <- modify this valuable
```

## Add your theme

If you want to add the theme you want, follow the below instruction.

1. Add the theme plugin in the plug.lua.

Search the pattern "/neovim color theme" and add you plugin into the curly brackets:

```lua
-- neovim color theme
use {
    'Avimitin/neovim-deus',
    'Shatur/neovim-ayu',
    'YOUR THEME',
}
```

2. Add the configuration for this theme in the colors.lua

Assuming that the name of your theme call "nord".

```lua
local theme = "nord"

local function nord_setup()
  -- Put your configuration here
  -- If you don't have configuration for the theme,
  -- leave it blank here.
end

local theme_opt = {
  ["nord"] = nord_setup,
}
```
