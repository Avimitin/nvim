-- theme
vim.opt.termguicolors=true
vim.opt.background="dark"
local theme = "deus"

local function ayu_setup()
  require('ayu').setup({
      mirage = true,
      overrides = {},
  })
end

local function deus_setup()
  vim.g.deus_background = "hard"
end

local theme_opt = {
  ["ayu"] = ayu_setup,
  ["deus"] = deus_setup,
}

theme_opt[theme]()

vim.cmd("colorscheme "..theme)
