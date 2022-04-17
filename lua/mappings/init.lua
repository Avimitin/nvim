local utils = require("mappings.utils")
local map = utils.map
local nmap = utils.nmap
local xmap = utils.xmap
local fmap = utils.fmap
local desc = utils.new_desc

vim.g.mapleader = " "

-- load plugin's keymapping
require("mappings.other")

-- quicker motion
nmap("J", "5j", desc("Jump 5 lines down"))
xmap("J", "5j", desc("Jump 5 lines down"))

nmap("K", "5k", desc("Jump 5 lines up"))
xmap("K", "5k", desc("Jump 5 lines up"))

nmap("L", "g_", desc("Jump to the end of the character"))
nmap("H", "^", desc("Jump to the beginning of the character"))

xmap("L", "g_", desc("Jump to the end of the character"))
xmap("H", "^", desc("Jump to the beginning of the character"))

nmap("W", "5w", desc("Jump 5 word forward"))
nmap("B", "5b", desc("Jump 5 word backward"))

-- no more background key
nmap("<C-z>", "u", desc("Revert change"))

-- move block easily
nmap("<", "<<", desc("Decrease indent"))
nmap(">", ">>", desc("Increase indent"))
xmap("<", "<gv", desc("Increase indent"))
xmap(">", ">gv", desc("Decrease indent"))

-- create tab like window
nmap("<C-T>h", ":tabprevious<CR>")
nmap("<C-T>l", ":tabnext<CR>")
nmap("<C-T>n", ":tabnew<CR>")

-- save quickly
nmap(";w", ":w<CR>", desc("Save buffer"))

-- kill buffer with ;q , quit window with :q . This make sense.
fmap("n", ";q", require("plugins.bufdel").delete_buffer)

-- do thing like ctrl c and ctrl v
xmap("<C-y>", [["+y]])
nmap("<C-p>", [["+p]])
map("i", "<C-p>", [[<ESC>"+pa]])

-- shut down the search high light
nmap("<ESC>", ":nohlsearch<CR>")
-- no more finger expansion
map("i", "<A-;>", "<ESC>", desc("Exit the insert mode"))

-- move around the window
nmap(";k", "<C-w>k")
nmap(";j", "<C-w>j")
nmap(";l", "<C-w>l")
nmap(";h", "<C-w>h")

-- resize the window
nmap("<C-S-up>", ":res +5<CR>")
nmap("<C-S-down>", ":res -5<CR>")
nmap("<C-S-right>", ":vertical resize-5<CR>")
nmap("<C-S-left>", ":vertical resize+5<CR>")

-- center editing line
map("i", "<C-c>", "<ESC>zzi")
