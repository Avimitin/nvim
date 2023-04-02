local map_utils = require("libs.keymap")
local cmd = map_utils.wrap_cmd

vim.g.mapleader = ";"

map_utils.nmap({
  { "j", "v:count == 0 ? 'gj' : 'j'", desc = "Go display lines downward", expr = true },
  { "k", "v:count == 0 ? 'gk' : 'k'", desc = "Go display lines upward", expr = true },
  { "J", "5j", desc = "Jump 5 lines down" },
  { "K", "5k", desc = "Jump 5 lines up" },
  { "L", "g_", desc = "Jump to beginning" },
  { "H", "^", desc = "Jump to end" },
  { "<C-z>", "u", desc = "Revert changes" },
  { "<", "<<", desc = "Decrease indent" },
  { ">", ">>", desc = "Increase indent" },
  { "<leader>w", cmd("w!"), desc = "Save buffer" },
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
})

if vim.cfg.core.nmaps and #vim.cfg.core.nmap > 0 then
  map_utils.nmap(vim.cfg.core.nmap)
end

map_utils.xmap({
  { "J", "5j", desc = "Select 5 lines down" },
  { "K", "5k", desc = "Select 5 lines up" },
  { "L", "g_", desc = "Select to beginning" },
  { "H", "^", desc = "Select to end" },
  { "<", "<gv", desc = "Increase indent" },
  { ">", ">gv", desc = "Decrease indent" },
})

map_utils.imap({
  { "<C-a>", "<ESC>^i", desc = "Jump to beginning of the line" },
  { "<C-e>", "<End>", desc = "Jump to end of the line" },
  { "<M-;>", "<ESC>", desc = "Exit insert mode" },
  { "<C-p>", "<Up>", desc = "Go up one line" },
  { "<C-f>", "<Right>", desc = "Go up one line" },
  { "<C-b>", "<Left>", desc = "Go up one line" },
  { "<C-n>", "<Down>", desc = "Go down one line" },
})

-- It is annoying to get escape code instead of space when I inputting capitalized English in terminal
map_utils.tmap({ "<S-Space>", "<Space>" })

-- Finally, let us handle user custom key mappings
for _, mappings in ipairs(vim.cfg.keymaps) do
  map_utils.map(mappings.mode or "n", mappings)
end
