local saga = require("lspsaga")
local themes = require("lspsaga.lspkind")
themes[12][2] = " "

-- use custom config
saga.init_lsp_saga({
  -- when cursor in saga window you config these to move
  move_in_saga = { prev = "k", next = "j" },
  diagnostic_header = { " ", " ", " ", " " },
  scroll_in_preview = {
    scroll_down = "<C-d>",
    scroll_up = "<C-u>",
  },
  code_action_icon = "ﯦ ",
  -- same as nvim-lightbulb but async
  code_action_lightbulb = {
    sign = false,
    virtual_text = true,
  },
  finder_icons = {
    def = "  ",
    ref = "  ",
    link = "  ",
  },
  finder_action_keys = {
    open = "<CR>",
    vsplit = "s",
    split = "i",
    tabe = "t",
    quit = "q",
    scroll_down = "<C-f>",
    scroll_up = "<C-b>", -- quit can be a table
  },
  -- show symbols in winbar must be neovim 0.8.0,
  -- close it until neovim 0.8.0 become stable
  symbol_in_winbar = {
    in_custom = false,
    enable = true,
    separator = "  ",
    show_file = true,
    click_support = false,
  },
})

vim.api.nvim_set_hl(0, "LspSagaLightBulb", { fg = "#E8C266" })
