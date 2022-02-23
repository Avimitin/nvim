local map = require("utils").map

vim.g.mapleader = " "

-- load plugin's keymapping
require("mappings.other")

-- quicker motion
map("n", "J", "5j")
map("x", "J", "5j")
map("n", "K", "5k")
map("x", "K", "5k")

map("n", "L", "g_")
map("n", "H", "^")
map("x", "L", "g_")
map("x", "H", "^")

map("n", "W", "5w")
map("n", "B", "5b")

-- no more background key
map("n", "<C-z>", "u")

-- move block easily
map("n", "<", "<<")
map("n", ">", ">>")
map("x", "<", "<gv")
map("x", ">", ">gv")

-- create tab like window
map("n", "<C-T>h", ":tabprevious<CR>")
map("n", "<C-T>l", ":tabnext<CR>")
map("n", "<C-T>n", ":tabnew<CR>")

-- save quickly
map("n", ";w", ":w<CR>")

-- kill buffer with ;q , quit window with :q . This make sense.
map("n", ";q", ":lua require('plugins.bufdel').delete_buffer()<CR>")

-- do thing like ctrl c and ctrl v
map("x", "<C-y>", [["+y]])
map("n", "<C-p>", [["+p]])
map("i", "<C-p>", [[<ESC>"+pa]])

-- shut down the search high light
map("n", "<ESC>", ":nohlsearch<CR>")

-- move around the window
map("n", ";k", "<C-w>k")
map("n", ";j", "<C-w>j")
map("n", ";l", "<C-w>l")
map("n", ";h", "<C-w>h")

-- resize the window
map("n", "<C-S-up>", ":res +5<CR>")
map("n", "<C-S-down>", ":res -5<CR>")
map("n", "<C-S-right>", ":vertical resize-5<CR>")
map("n", "<C-S-left>", ":vertical resize+5<CR>")

-- center editing line
map("i", "<C-c>", "<ESC>zzi")
