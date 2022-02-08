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

map("n", "<C-T>h", ":tabprevious<CR>")
map("n", "<C-T>l", ":tabnext<CR>")
map("n", "<C-T>n", ":tabnew<CR>")

vim.g.mapleader = " "

map("n", ";w", ":w<CR>")
-- quit needs user to confirm
map("n", ";q", ":Sayonara<CR>")

map("x", "<C-y>", [["+y]])

map("n", "<C-p>", [["+p]])
map("i", "<C-p>", [[<ESC>"+pa]])

map("n", "<ESC>", ":nohlsearch<CR>")

map("n", ";k", "<C-w>k")
map("n", ";j", "<C-w>j")
map("n", ";l", "<C-w>l")
map("n", ";h", "<C-w>h")

map("n", "<C-S-up>", ":res +5<CR>")
map("n", "<C-S-down>", ":res -5<CR>")
map("n", "<C-S-right>", ":vertical resize-5<CR>")
map("n", "<C-S-left>", ":vertical resize+5<CR>")

-- center line
map("i", "<C-c>", "<ESC>zzi")

-- EasyAlign
map("v", "<leader>e", ":EasyAlign<CR>")

-- nvim-tree
map("n", ";t", ":NvimTreeToggle<CR>")

map("n", "<C-\\>", [[:FTermToggle<CR>]])
map("t", "<C-\\>", [[<C-\><C-n>:FTermToggle<CR>]])
map("t", "<C-n>", [[<C-\><C-n>]])

map("n", ";f", [[:lua require('telescope.builtin').find_files{}<CR>]])
map("n", "<LEADER>tg", [[:lua require('telescope.builtin').live_grep{}<CR>]])

map("n", ";g", [[<CMD>Git<CR>]])
map("n", "<LEADER>l", [[<CMD>LazyGit<CR>]])

map("i", "<A-;>", "<ESC>")

-- bufferline tab stuff
map("n", "<C-c>", ":BufferLinePickClose<CR>") -- close tab

-- move between tabs
map("n", ";n", [[<Cmd>BufferLineCycleNext<CR>]])
map("n", ";p", [[<Cmd>BufferLineCyclePrev<CR>]])

-- move tabs
map("n", "<A->>", [[<CMD>BufferLineMoveNext<CR>]])
map("n", "<A-<>", [[<CMD>BufferLineMovePrev<CR>]])
map("n", "<A-p>", [[<CMD>:BufferLinePick<CR>]])

map("n", ";d", ":Dispatch ", {noremap = true, silent = false})
