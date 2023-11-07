local cmd = require("builder.key-mapper").cmd
local keymapper = require("builder.key-mapper")

keymapper.mk_keymap({
  mapleader = {
    global = ";",
  },
  normal = {
    { "j", "v:count == 0 ? 'gj' : 'j'", desc = "Go display lines downward", expr = true },
    { "k", "v:count == 0 ? 'gk' : 'k'", desc = "Go display lines upward", expr = true },
    { "L", "g_", desc = "Jump to beginning" },
    { "H", "^", desc = "Jump to end" },
    { "<C-z>", "u", desc = "Revert changes" },
    { "<leader>w", cmd("silent w!"), desc = "Save buffer" },
    { "<ESC>", cmd("noh"), desc = "Close search highlight" },
    {
      "<leader>q",
      function()
        require("libs.bufdel").delete_buffer_expr("", false)
      end,
      desc = "Close current buffer",
    },
    { "<leader>x", cmd("x"), desc = "Save and quit" },
    { "<C-p>", [["+p]], desc = "paste" },
  },
  selection = {
    { "L", "g_", desc = "Select to beginning" },
    { "H", "^", desc = "Select to end" },
    { "<C-z>", "<nop>", desc = "Revert changes" },
    { "<tab>", ">gv", desc = "Increase indent" },
    { "<s-tab>", "<gv", desc = "Decrease indent" },
  },
  insertion = {
    { "<C-a>", "<ESC>^i", desc = "Jump to beginning of the line" },
    { "<C-e>", "<End>", desc = "Jump to end of the line" },
    { "<M-;>", "<ESC>", desc = "Exit insert mode" },
    { "<C-p>", "<Up>", desc = "Go up one line" },
    { "<C-f>", "<Right>", desc = "Go up one line" },
    { "<C-b>", "<Left>", desc = "Go up one line" },
    { "<C-n>", "<Down>", desc = "Go down one line" },
    { "<C-z>", "<esc>ui", desc = "Revert changes" },
  },
  terminal = {
    { "<S-Space>", "<Space>" },
  },
})
