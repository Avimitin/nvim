# Colors

This section describe how I manage the colorscheme.

## Config path

```text
lua/core/colors.lua
```

## Change the theme

1. **Change default value**

This configuration has "kanagawa" as the default colorscheme value.
If you want to change the default value, you can modify the `default_value` variable.

```lua
local default_theme = "kanagawa" -- <- change this value
```

2. **Change local value**

There is also another mechanism to modify this value.
You can create a file named `custom.lua` under the lua directory.
Then return a table value in the below form:

```lua
-- ~/.config/nvim/lua/custom.lua

local M = {
  theme = "deus"
}

return M
```

The `custom.lua` file is ignored by git, so you can change the colorscheme whatever you like.

## Gallery

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

* nightfox

![nightfox-image](https://user-images.githubusercontent.com/2746374/158456286-9e3ee657-60e6-49d8-b85e-dcab285b31c3.png) 

* dawnfox

![dawnfox-image](https://user-images.githubusercontent.com/2746374/158456278-c5d656de-c445-44b8-9813-9fc91ffbce4c.png) 

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
