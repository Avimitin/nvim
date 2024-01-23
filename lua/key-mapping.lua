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
    {
      "<leader>w",
      function()
        vim.cmd.write({ bang = true })
      end,
      desc = "Save buffer",
    },
    { "<ESC>", vim.cmd.noh, desc = "Close search highlight" },
    {
      "<leader>q",
      function()
        require("libs.bufdel").delete_buffer_expr("", false)
      end,
      desc = "Close current buffer",
    },
    { "<leader>x", vim.cmd.x, desc = "Save and quit" },
    { "<C-p>", [["+p]], desc = "paste" },
    { "<C-d>", "<C-d>zz", desc = "paste" },
    { "<C-u>", "<C-u>zz", desc = "paste" },

    -- tools
    {
      "<leader>b",
      function()
        require("buffer_manager.ui").toggle_quick_menu()
      end,
      desc = "Toggle buffer manager",
    },
    {
      "<Tab>",
      function()
        require("cybu").cycle("next")
      end,
      desc = "Next buffer",
    },
    {
      "<S-Tab>",
      function()
        require("cybu").cycle("prev")
      end,
      desc = "Prev buffer",
    },
  },
  selection = {
    { "J", ":m '>+1<CR>gv=gv" },
    { "K", ":m '<-2<CR>gv=gv" },
    { "L", "g_", desc = "Select to beginning" },
    { "H", "^", desc = "Select to end" },
    { "<C-z>", "<nop>", desc = "anti-touch" },
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
    { "<C-z>", "<nop>", desc = "anti-touch" },
  },
  terminal = {
    { "<S-Space>", "<Space>" },
  },
})
