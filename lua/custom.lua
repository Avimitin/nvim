-- This file is ignore by git by default

local my_config = {
  -- the global theme settings
  theme = "kanagawa",

  -- this fields will not be use by editor, but can help us setting the theme fields
  auto_darkmode = {
    enable = true,
    day_theme = "github_light",
    night_theme = "kanagawa",
    time = {
      begin = "18:30",
      ending = "7:00",
    },
  },

  -- enable fcitx5 auto toggle?
  auto_toggle_fcitx5 = true,

  -- Single string or a array with one item represent filetype that
  -- nvim-treesitter should load.
  --
  -- Array with two item which sencond item is lsp server means both
  -- lspconfig and nvim-treesitter should be load on this filetype,
  -- and lspconfig will automatically install the server.
  langs = {
    "bash",
    "comment",
    "fish",
    "html",
    "json",
    "nix",
    "rust",
    "toml",
    { "vim" },
    { "c", "clangd" },
    { "cpp", "clangd" },
    { "go", "gopls" },
    { "javascript", "eslint" },
    { "lua", "sumneko_lua" },
    { "python", "pyright" },
  },

  -- enable vale? Should install vale before setting this to true
  enable_vale = false,
}

-- update theme base on current time
local function update_theme()
  -- try to update the theme by current time
  if my_config.auto_darkmode and my_config.auto_darkmode.enable then
    local darkmode_setting = my_config.auto_darkmode
    if darkmode_setting.time == nil then
      vim.notify("Darkmode enabled but no time range given")
      return
    end
    if darkmode_setting.day_theme == nil or darkmode_setting.enable == nil then
      vim.notify("Darkmode enabled but no day theme or night theme are given")
      return
    end

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

    local begin = parse_time(darkmode_setting.time.begin)
    local ending = parse_time(darkmode_setting.time.ending)
    local now = parse_time(os.date("%H:%M"))

    if ending < begin then
      -- add 24 hour
      ending = ending + 72000
    end

    if (now >= begin) and (now <= ending) then
      my_config.theme = darkmode_setting.night_theme
    else
      my_config.theme = darkmode_setting.day_theme
    end
  end
end

update_theme()

return my_config
