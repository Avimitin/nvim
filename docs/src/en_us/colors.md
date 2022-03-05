# Colors

This section describe how I manage the colorscheme.

## Config path

```text
lua/core/colors.lua
```

## Change the theme

You can modify the variable `M.theme` in `colors.lua` file.
Currently, we have:

* deus

```lua
M.theme = "deus"
```

![images](https://raw.githubusercontent.com/Avimitin/nvim/master/docs/images/deus.png)

* kanagawa 

```lua
M.theme = "kanagawa"
```

![images](https://raw.githubusercontent.com/Avimitin/nvim/master/docs/images/kanagawa.png)

## Add your theme

If you want to add the theme you prefer, follow the below instruction.

1. Add the theme plugin in the plug.lua.

Assuming that the name of your theme call "nord".

```lua
use {
  'somebody/nord',
  cond = function()
    return require("colors").theme == "nord"
  end,
  config = function()
    require("colors").nord_setup()
  end
}
```

You can read the lua/plugins.lua file for example.

2. Add the configuration in the colors.lua

```lua
-- select the theme
M.theme = "nord"

M.nord_setup = function()
  -- leave it blank here if you don't have any config
  vim.g.background = "dark"

  -- And don't forget this step, it is the final step to load the theme
  set_color("nord")
end
```
