local map = require('utils').map

map('n', 'J', '5j')
map('x', 'J', '5j')
map('n', 'K', '5k')
map('x', 'K', '5k')

map("n", "L", "g_")
map("n", "H", "^")
map("x", "L", "g_")
map("x", "H", "^")

map("n", "W", "5w")
map("n", "B", "5b")

map("n", "<C-z>", "u")

map("n", "<", "<<")
map("n", ">", ">>")
map("x", "<", "<gv")
map("x", ">", ">gv")

map("n", "-", "N")
map("n", "=", "n")
map("n", ";", ":")

map("n", "<C-T>h", ":tabprevious<CR>")
map("n", "<C-T>l", ":tabnext<CR>")
map("n", "<C-T>n", ":tabnew<CR>")

vim.g.mapleader = " "

map("n", ";w", ":w<CR>")

map("x", "<C-y>", [["+y]])

map("n", "<C-p>", [["+p]])
map("i", "<C-p>", [[<ESC>"+pi]])

map("n", "<ESC>", ":nohlsearch<CR>")

map("n", "<up>", ":res +5<CR>")
map("n", "<down>", ":res -5<CR>")
map("n", "<left>", ":vertical resize-5<CR>")
map("n", "<right>", ":vertical resize+5<CR>")

-- center line
map("i", "<C-c>", "<ESC>zzi")

-- nnn
map('n', '<Leader>o', ':NnnPicker %:p:h<CR>')

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

map("i", "<A-;>", "<ESC>")

-- bufferline tab stuff
map("n", "<A-t>", ":tabnew<CR>") -- new tab
map("n", "<C-c>", ":BufferLinePickClose<CR>") -- close tab
map("n", "<A-q>", [[:Sayonara<CR>]])

-- move between tabs
map("n", ".", [[<Cmd>BufferLineCycleNext<CR>]])
map("n", ",", [[<Cmd>BufferLineCyclePrev<CR>]])

-- move tabs
map("n", "<A->>", [[<CMD>BufferLineMoveNext<CR>]])
map("n", "<A-<>", [[<CMD>BufferLineMovePrev<CR>]])
map("n", "<A-p>", [[<CMD>:BufferLinePick<CR>]])
