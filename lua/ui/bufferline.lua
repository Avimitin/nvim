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

local cmd = require("libs.keymap").wrap_cmd
require("libs.keymap").nmap({
  { "<C-c>", cmd("BufferLinePickClose"), desc = "Close buffer" },
  { "<Tab>", cmd("BufferLineCycleNext"), desc = "Goto next buffer" },
  { "<S-Tab>", cmd("BufferLineCyclePrev"), desc = "Goto previous buffer" },
  { "<Leader>p", cmd("BufferLinePick"), desc = "Pick a buffer" },
  -- This might pollute user tmux/terminal settings
  { "<M-S-right>", cmd("BufferLineMoveNext"), desc = "Move buffer to right" },
  { "<M-S-left>", cmd("BufferLineMovePrev"), desc = "Move buffer to left" },
})
