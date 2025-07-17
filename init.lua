-- Reject loading plugins when bigfile detect. Default on 1.5M size.
require("libs.bigfile").setup()

require("core")
require("pack").setup()

require("keys").mk_keymap({
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
    { "<C-S-v>", [["+p]], desc = "paste" },
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

vim.o.background = "dark"
vim.cmd.colorscheme("kanagawa")

require("lang").setup_lsp()
