# Colors

This section describe how I manage the colorscheme.

## Config path

```text
lua/core/colors.lua
```

## Change the theme

1. **Change default value**

This configuration has "kanagawa" as the default colorscheme value.
If you want to change the default value, you can modify the `M.theme` variable.

```lua
local M = {
  theme = "kanagawa" -- <- change this value
}
```

2. **Change local value**

There is also another mechanism to modify this value.
You can create a file named `custom.lua` under the `lua` directory.
Then return a table value in the below form:

```lua
-- ~/.config/nvim/lua/custom.lua

local M = {
  theme = "deus"
}

return M
```

The `custom.lua` file is ignored by git, so you can change the colorscheme whatever you like.

Additionally, with this custom script file, you can switch day and night theme
automatically.

Example script:

```lua
-- ~/.config/nvim/lua/custom.lua

local M = {
  -- leave this unchange
  theme = "",
  -- Here define the colorscheme for day
  day_theme = "github_light",
  -- Here define the colorscheme for night
  night_theme = "kanagawa",
  -- Here setup night mode duration with format "Hour:Min"
  darkmode_time = {
    begin = "19:00",
    ending = "7:00",
  },
}

-- This function will parse our string time to a table to a unix timestamp
local function parse_time(str)
  if str then
    local hour, min = str:match("(%d+):(%d+)")
    return os.time({
      hour = hour,
      min = min,
      day = 1,
      month = 1,
      year = 1970,
    })
  end
end

-- if user setup the darkmode related fields
if M.darkmode_time and M.darkmode_time.begin and M.darkmode_time.ending then
  local begin = parse_time(M.darkmode_time.begin)
  local ending = parse_time(M.darkmode_time.ending)
  local now = parse_time(os.date("%H:%M"))

  -- we might want the 7:00 in next day
  if ending < begin then
    -- add 24 hour
    ending = ending + 72000
  end

  -- if the night has come
  if (now >= begin) and (now <= ending) then
    M.theme = M.night_theme
  else
    M.theme = M.day_theme
  end
end

return M
```

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

* GitHub

![GitHub-image](https://camo.githubusercontent.com/4bb7ad6c319b5ce63bed16cb25753e603fee510d59a1fad0245bc3d0bda8445d/68747470733a2f2f696d6775722e636f6d2f4f5077424449342e706e67)

## Add your theme

If you want to add the theme you prefer, follow the below instruction.

1. Add the theme plugin in the `load.lua`.

Assuming that the name of your theme call "nord".

```lua
{
  'somebody/nord',
  cond = function()
    return require("colors").theme == "nord"
  end,
  config = function()
    require("colors").nord_setup()
  end
}
```

You can read the `lua/plugins/load.lua` file for example.

2. Add the configuration in the colors.lua

```lua
-- select the theme
M.theme = "nord"

M.nord_setup = function()
  -- leave it blank here if you don't have any config
  vim.g.background = "dark"

  -- And don't forget this step, it is the final step to load the theme
  apply()
end
```
