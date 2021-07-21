local function map(mode, lhs, rhs, opts)
    local options = {noremap = true, silent = true}
    if opts then
        options = vim.tbl_extend("force", options, opts)
    end
    local stat = pcall(vim.api.nvim_set_keymap, mode, lhs, rhs, options)
		if not stat then
			print(lhs)
			print(rhs)
			print("illegal")
		end
end

map("",  "K",     "5k")
map("",  "J",     "5j")
map("",  "L",     "$")
map("",  "H",     "0")
map("",  "X",     "Vx")
map("",  "W",     "5w")
map("",  "B",     "5b")
map("",  "vw",    "viw")
map("",  "<C-z>", "u")
map("n", "<",     "<<")
map("n", ">",     ">>")
map("",  "s",     "<nop>")
map("",  "-",     "N")
map("",  "=",     "n")

vim.g.mapleader=" "

map("n", "<LEADER>s", ":w<CR>")
map("n", "<C-s>",     ":w<CR>")

map("n", "<LEADER>q", ":wq<CR>")

map("n", "<C-q>",     ":q<CR>")

map("v", "<LEADER>y", [["+y]])

map("",  "<LEADER>p", [["+p]])

map("n", "<ESC>",     ":nohlsearch<CR>")

map("i", "jj",        "<ESC>")
map("v", "jj",        "<ESC>")

map("n", "spv",       "<C-w>t<C-w>H")

map("n", "srr",       "<C-w>b<C-w>K")
map("n", "srv",       "<C-w>b<C-w>H")

map("n", "<up>",      ":res +5<CR>")
map("n", "<down>",    ":res -5<CR>")
map("n", "<left>",    ":vertical resize-5<CR>")
map("n", "<right>",   ":vertical resize+5<CR>")

map("n", "sk",        "<C-w>k")
map("n", "sj",        "<C-w>j")
map("n", "sh",        "<C-w>h")
map("n", "sl",        "<C-w>l")

map("i", "<C-c>",     "<ESC>zzi")
