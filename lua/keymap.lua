local map = require('utils').map

map('n', 'J', '5j')
map('x', 'J', '5j')
map('n', 'K', '5k')
map('x', 'K', '5k')
map("n", "L", "g_")
map("n", "H", "^")
map("x", "L", "g_")
map("i", "<C-l>", "<ESC>A")
map("x", "H", "^")
map("n", "X", "Vx")
map("n", "W", "5w")
map("n", "B", "5b")
map("n", "<C-z>", "u")
map("n", "<", "<<")
map("n", ">", ">>")
map("x", "<", "<gv")
map("x", ">", ">gv")
map("n", "s", "<nop>")
map("n", "-", "N")
map("n", "=", "n")
map("n", ";", ":")

map("n", "<C-T>h", ":tabprevious<CR>")
map("n", "<C-T>l", ":tabnext<CR>")
map("n", "<C-T>n", ":tabnew<CR>")

vim.g.mapleader = " "

map("n", ";w", ":w<CR>")

map("n", "<C-A-q>", ":qa<CR>")

map("x", "<LEADER>y", [["+y]])

map("n", "<LEADER>p", [["+p]])

map("n", "<ESC>", ":nohlsearch<CR>")

map("i", "<A-;>", "<ESC>")

map("n", "<up>", ":res +5<CR>")
map("n", "<down>", ":res -5<CR>")
map("n", "<left>", ":vertical resize-5<CR>")
map("n", "<right>", ":vertical resize+5<CR>")

-- center line
map("i", "<C-c>", "<ESC>zzi")

-- nnn
map('n', '<Leader>o', ':NnnPicker %:p:h<CR>')

-- hop
map('n', 'f', ':HopChar2<CR>')
map('x', 'f', '<CMD>HopChar2<CR>')
map('n', '<C-J>', ':HopLine<CR>')

-- EasyAlign
map("v", "<leader>e", ":EasyAlign<CR>")

-- vim-go
map('n', 'got', ':GoTestFunc<CR>')
map('n', 'gor', ':GoRun<CR>')

-- nvim-tree
map("n", "tt", ":NvimTreeToggle<CR>")
map("n", "tr", ":NvimTreeRefresh<CR>")

map("n", "<C-\\>", [[:FTermToggle<CR>]])
map("t", "<C-\\>", [[<C-\><C-n>:FTermToggle<CR>]])
map("t", "<C-n>", [[<C-\><C-n>]])

map("n", "<LEADER>tf", [[:lua require('telescope.builtin').find_files{}<CR>]])
map("n", "<LEADER>tg", [[:lua require('telescope.builtin').live_grep{}<CR>]])

map("n", "<LEADER>ng", [[<CMD>Neogit<CR>]])
map("n", "<LEADER>lg", [[<CMD>LazygitToggle<CR>]])
