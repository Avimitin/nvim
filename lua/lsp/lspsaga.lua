local saga = require("lspsaga")

-- use custom config
saga.setup({
  -- when cursor in saga window you config these to move
  move_in_saga = { prev = "k", next = "j" },
  diagnostic_header = { " ", " ", " ", " " },
  scroll_preview = {
    scroll_down = "<C-d>",
    scroll_up = "<C-u>",
  },
  -- same as nvim-lightbulb but async
  lightbulb = {
    sign = false,
    enable_in_insert = false,
    virtual_text = true,
  },
  symbol_in_winbar = {
    enable = true,
    separator = "  ",
    show_file = true,
  },
  ui = {
    code_action = "ﯦ",
    diagnostic = "",
    preview = "",
  },
})

vim.api.nvim_set_hl(0, "SagaLightBulb", { fg = "#E8C266" })
