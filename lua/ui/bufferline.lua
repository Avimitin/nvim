local preset = require("bufferline").style_preset

require("bufferline").setup({
  options = {
    style_preset = {
      preset.no_italic,
      preset.no_bold,
    },
    offsets = { { filetype = "neo-tree", text = " NeoTree" } },
    buffer_close_icon = "",
    modified_icon = "",
    max_name_length = 14,
    max_prefix_length = 13,
    tab_size = 20,
    diagnostic = false,
    show_tab_indicators = true,
    enforce_regular_tabs = false,
    view = "multiwindow",
    show_buffer_close_icons = true,
    separator_style = { "", "" },
    always_show_bufferline = true,
    indicator = { style = "none" },
    groups = {
      options = {
        toggle_hidden_on_enter = false, -- when you re-enter a hidden group this options re-opens that group so the buffer is visible
      },
      items = {
        {
          name = " Terminals",
          auto_close = true,
          matcher = function(buf)
            return buf.buftype:match("terminal")
          end,
        },
      },
    },
  },
})

local cmd = require("libs.keymap").wrap_cmd
require("libs.keymap").nmap({
  { "<Tab>", cmd("BufferLineCycleNext"), desc = "Goto next buffer" },
  { "<S-Tab>", cmd("BufferLineCyclePrev"), desc = "Goto previous buffer" },
  { "<A-p>", cmd("BufferLinePick"), desc = "Pick a buffer" },
  { "<<", cmd("BufferLineMovePrev"), desc = "Move current buffer to previous one" },
  { ">>", cmd("BufferLineMoveNext"), desc = "Move current buffer to previous one" },
})
