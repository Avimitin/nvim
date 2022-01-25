# 颜色方案

本节描述了我是如何管理颜色方案的。

## 配置文件

```text
lua/colors.lua
```

## 更改颜色方案

你可以在 `colors.lua` 文件中修改变量 `M.theme`。
目前，我们有:

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

* everforest 

```lua
M.theme = "everforest"
```

![images](https://raw.githubusercontent.com/Avimitin/nvim/master/docs/images/everforest.png) 

* ayu 

```lua
M.theme = "ayu"
```

![images](https://raw.githubusercontent.com/Avimitin/nvim/master/docs/images/ayu.png) 

* gruvbox 

```lua
M.theme = "gruvbox"
```

![images](https://raw.githubusercontent.com/Avimitin/nvim/master/docs/images/gruvbox.png) 

## 添加你的主题

如果你想添加你喜欢的主题，请按照下面的指示。

1. 添加主题插件到 plug.lua。

假设你想要添加的主题名称叫 "nord"。

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

你可以阅读 lua/plugins.lua 文件为例。

2. 在 color.lua 中添加配置

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
