local config = {}

-- pre-process
config.pre = function()
  if not vim.fn.has("nvim-0.6") then
    -- for filetype.nvim
    -- If using a Neovim version earlier than 0.6.0
    vim.g.did_load_filetypes = 1
  end

  -- for wildfire
  vim.g.wildfire_objects = { "i'", 'i"', "i)", "i]", "i}", "ip", "it", "i`" }

  vim.g.rooter_manual_only = 1
  vim.g.rooter_change_directory_for_non_project_files = "current"
  vim.g.rooter_patterns = {
    ".git",
    "Cargo.toml",
    "package.json",
    "tsconfig.json",
  }

  -- visual multi mappings
  -- clean the keymap `u` and initialize the new keymap set
  require("editor.utils").map("", "u", "<nop>")

  -- u is map to <C-z>, let us reuse it here
  vim.g.VM_maps = {
    ["Find Under"] = "\\n",
    ["Find Subword Under"] = "\\n",
    ["Select Cursor Down"] = "<C-down>",
    ["Select Cursor Up"] = "<C-up>",
    ["Select All"] = "uA",
    ["Undo"] = "<C-z>",
    ["Redo"] = "<C-r>",
    ["Start Regex Search"] = "ux",
    ["Visual Regex"] = "ux",
    ["Visual All"] = "uA",
    ["Visual Add"] = "ua",
    ["Visual Find"] = "uf",
    ["Visual Cursors"] = "uc",
  }
end

config.post = function()
  require("plugins.enhance.config.rooter")
end

--
-- re-export the inner configuration
--
local function load(name)
  return require("plugins.enhance.config." .. name)
end

config.autopairs_config = load("autopairs")
config.whichkey_config = load("which-key")
config.galaxyline_config = load("galaxyline")
config.indent_config = load("indent")
config.nvim_tree_config = load("nvim-tree")
config.telescope_config = load("telescope")

config.vfiler_config = function()
  require("vfiler/config").setup({
    options = {
      columns = "indent,devicons,name,mode,size,time",
    },
  })
end

config.bufferline_cfg = function()
  require("bufferline").setup({
    options = {
      offsets = { { filetype = "NvimTree", text = " Explorer", padding = 1 } },
      buffer_close_icon = "",
      modified_icon = "",
      close_icon = "",
      left_trunc_marker = "",
      right_trunc_marker = "",
      max_name_length = 14,
      max_prefix_length = 13,
      tab_size = 20,
      diagnostic = false,
      show_tab_indicators = true,
      enforce_regular_tabs = false,
      view = "multiwindow",
      show_buffer_close_icons = true,
      separator_style = "slant",
      always_show_bufferline = true,
    },
  })
end

config.dashboard_cfg = function()
  local db = require("dashboard")
  db.custom_header = {
    [[      ___                                    ___     ]],
    [[     /__/\          ___        ___          /__/\    ]],
    [[     \  \:\        /__/\      /  /\        |  |::\   ]],
    [[      \  \:\       \  \:\    /  /:/        |  |:|:\  ]],
    [[  _____\__\:\       \  \:\  /__/::\      __|__|:|\:\ ]],
    [[ /__/::::::::\  ___  \__\:\ \__\/\:\__  /__/::::| \:\]],
    [[ \  \:\~~\~~\/ /__/\ |  |:|    \  \:\/\ \  \:\~~\__\/]],
    [[  \  \:\  ~~~  \  \:\|  |:|     \__\::/  \  \:\      ]],
    [[   \  \:\       \  \:\__|:|     /__/:/    \  \:\     ]],
    [[    \  \:\       \__\::::/      \__\/      \  \:\    ]],
    [[     \__\/           ~~~~                   \__\/    ]],
  }
  require("telescope")
  db.custom_center = {
    {
      icon = "  ",
      desc = "Create new file                         ",
      action = "DashboardNewFile",
      shortcut = "SPC f d",
    },
    {
      icon = "  ",
      desc = "Recently opened files                   ",
      action = "Telescope oldfiles",
      shortcut = "SPC f h",
    },
    {
      icon = "  ",
      desc = "Find  File                              ",
      action = "Telescope find_files find_command=rg,--hidden,--files",
      shortcut = "SPC f f",
    },
    {
      icon = "  ",
      desc = "File Browser                            ",
      action = "NvimTreeToggle",
      shortcut = "SPC f b",
    },
    {
      icon = "  ",
      desc = "Find  word                              ",
      action = "Telescope live_grep",
      shortcut = "SPC f w",
    },
  }
end

config.neoscroll_config = function()
  require("neoscroll").setup({
    -- All these keys will be mapped to their corresponding default scrolling animation
    mappings = {
      "<C-j>",
      "<C-k>",
      "<C-b>",
      "<C-f>",
      "<C-y>",
      "<C-e>",
      "zt",
      "zz",
      "zb",
    },
    hide_cursor = true, -- Hide cursor while scrolling
    stop_eof = true, -- Stop at <EOF> when scrolling downwards
    use_local_scrolloff = false, -- Use the local scope of scrolloff instead of the global scope
    respect_scrolloff = false, -- Stop scrolling when the cursor reaches the scrolloff margin of the file
    cursor_scrolls_alone = true, -- The cursor will keep on scrolling even if the window cannot scroll further
    easing_function = "circular", -- Default easing function
    pre_hook = nil, -- Function to run before the scrolling animation starts
    post_hook = nil, -- Function to run after the scrolling animation ends
    performance_mode = false, -- Disable "Performance Mode" on all buffers.
  })

  local t = {}
  -- Syntax: t[keys] = {function, {function arguments}}
  t["<C-j>"] = { "scroll", { "vim.wo.scroll", "true", "350" } }
  t["<C-k>"] = { "scroll", { "-vim.wo.scroll", "true", "350" } }
  t["<C-b>"] = { "scroll", { "-vim.api.nvim_win_get_height(0)", "true", "150" } }
  t["<C-f>"] = { "scroll", { "vim.api.nvim_win_get_height(0)", "true", "150" } }
  t["<C-y>"] = { "scroll", { "-0.10", "false", "100" } }
  t["<C-e>"] = { "scroll", { "0.10", "false", "100" } }
  t["zt"] = { "zt", { "150" } }
  t["zz"] = { "zz", { "150" } }
  t["zb"] = { "zb", { "150" } }

  require("neoscroll.config").set_mappings(t)
end

config.toggleterm_config = function()
  require("toggleterm").setup({
    -- size can be a number or function which is passed the current terminal
    size = function(term)
      if term.direction == "horizontal" then
        return 15
      elseif term.direction == "vertical" then
        return vim.o.columns * 0.4
      end
    end,
    hide_numbers = true,
    shade_filetypes = {},
    shade_terminals = true,
    start_in_insert = false,
    insert_mappings = true, -- whether or not the open mapping applies in insert mode
    terminal_mappings = true, -- whether or not the open mapping applies in the opened terminals
    persist_size = true,
    direction = "horizontal", -- 'window' | 'float' | 'vertical' ,
    close_on_exit = true, -- close the terminal window when the process exits
    shell = vim.o.shell, -- change the default shell
    float_opts = {
      border = "single",
      winblend = 3,
      highlights = {
        border = "Normal",
        background = "Normal",
      },
    },
  })
end

return config
