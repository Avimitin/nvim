-- This file is ignore by git by default

local my_config = {
  -- the global theme settings
  theme = "kanagawa",

  -- this fields will not be use by editor, but can help us setting the theme fields
  day_theme = "github_light",
  night_theme = "kanagawa",
  darkmode_time = {
    begin = "18:30",
    ending = "7:00",
  },

  -- enable fcitx5 auto toggle?
  has_fcitx5 = true,

  -- lspconfig settings
  lspconfig = {
    -- required language servers
    servers = {
      "sumneko_lua",
      "gopls",
      "eslint",
      "clangd",
      "pyright",
    },

    -- enabled lspconfig for what filetype?
    ft = {
      "python",
    },
  },

  -- treesitter settings
  treesitter = {
    -- enable treesitter for what filetype?
    language = {
      "bash",
      "c",
      "comment",
      "cpp",
      "fish",
      "go",
      "html",
      "javascript",
      "json",
      "lua",
      "nix",
      "rust",
      "toml",
      "python",
      "vim",
    },
  },

  -- enable vale? Should install vale before setting this to true
  enable_vale = false,
}

-- try to update the theme by current time
if my_config.darkmode_time and my_config.darkmode_time.begin and my_config.darkmode_time.ending then
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

  local begin = parse_time(my_config.darkmode_time.begin)
  local ending = parse_time(my_config.darkmode_time.ending)
  local now = parse_time(os.date("%H:%M"))

  if ending < begin then
    -- add 24 hour
    ending = ending + 72000
  end

  if (now >= begin) and (now <= ending) then
    my_config.theme = my_config.night_theme
  else
    my_config.theme = my_config.day_theme
  end
end

return my_config
