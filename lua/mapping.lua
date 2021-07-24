function Map(mode, lhs, rhs, opts)
    local options = {noremap = true, silent = true}
    if opts then
        options = vim.tbl_extend("force", options, opts)
    end
    local stat, error = pcall(vim.api.nvim_set_keymap, mode, lhs, rhs, options)
		if not stat then
			print(error)
		end
end

Map("",  "K",     "5k")
Map("",  "J",     "5j")
Map("",  "L",     "$")
Map("",  "H",     "0")
Map("",  "X",     "Vx")
Map("",  "W",     "5w")
Map("",  "B",     "5b")
Map("",  "vw",    "viw")
Map("",  "<C-z>", "u")
Map("n", "<",     "<<")
Map("n", ">",     ">>")
Map("",  "s",     "<nop>")
Map("",  "-",     "N")
Map("",  "=",     "n")

vim.g.mapleader=" "

Map("n", "<LEADER>s", ":w<CR>")
Map("n", "<C-s>",     ":w<CR>")

Map("n", "<LEADER>q", ":wq<CR>")

Map("n", "<C-q>",     ":q<CR>")

Map("v", "<LEADER>y", [["+y]])

Map("",  "<LEADER>p", [["+p]])

Map("n", "<ESC>",     ":nohlsearch<CR>")

Map("i", "jj",        "<ESC>")

Map("n", "spv",       "<C-w>t<C-w>H")

Map("n", "srr",       "<C-w>b<C-w>K")
Map("n", "srv",       "<C-w>b<C-w>H")

Map("n", "<up>",      ":res +5<CR>")
Map("n", "<down>",    ":res -5<CR>")
Map("n", "<left>",    ":vertical resize-5<CR>")
Map("n", "<right>",   ":vertical resize+5<CR>")

Map("n", "sk",        "<C-w>k")
Map("n", "sj",        "<C-w>j")
Map("n", "sh",        "<C-w>h")
Map("n", "sl",        "<C-w>l")

Map("i", "<C-c>",     "<ESC>zzi")

--nnn
Map('n', '<Leader>o', ':NnnPicker %:p:h<CR>')
