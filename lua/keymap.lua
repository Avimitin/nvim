function Map(mode, lhs, rhs, opts)
    local options = {noremap = true, silent = true}
    if opts then options = vim.tbl_extend("force", options, opts) end
    local stat, error = pcall(vim.api.nvim_set_keymap, mode, lhs, rhs, options)
    if not stat then vim.notify(error, vim.log.levels.ERROR, {title='keymap'}) end
end

Map('n', 'J', '5j')
Map('n', 'K', '5k')
Map("n", "L", "g_")
Map("n", "H", "^")
Map("x", "L", "g_")
Map("x", "H", "^")
Map("n", "X", "Vx")
Map("n", "W", "5w")
Map("n", "B", "5b")
Map("n", "vw", "viw")
Map("n", "<C-z>", "u")
Map("n", "<", "<<")
Map("n", ">", ">>")
Map("x", "<", "<gv")
Map("x", ">", ">gv")
Map("n", "s", "<nop>")
Map("n", "-", "N")
Map("n", "=", "n")
Map("n", ";", ":")

Map("n", "<C-T>h", ":tabprevious<CR>")
Map("n", "<C-T>l", ":tabnext<CR>")
Map("n", "<C-T>n", ":tabnew<CR>")

vim.g.mapleader = " "

Map("n", ";w", ":w<CR>")

Map("n", "<C-A-q>", ":qa<CR>")

Map("x", "<LEADER>y", [["+y]])

Map("n", "<LEADER>p", [["+p]])

Map("n", "<ESC>", ":nohlsearch<CR>")

Map("i", "jj", "<ESC>")

Map("n", "<up>", ":res +5<CR>")
Map("n", "<down>", ":res -5<CR>")
Map("n", "<left>", ":vertical resize-5<CR>")
Map("n", "<right>", ":vertical resize+5<CR>")

-- center line
Map("i", "<C-c>", "<ESC>zzi")

-- nnn
Map('n', '<Leader>o', ':NnnPicker %:p:h<CR>')

-- hop
Map('n', 'f', ':HopChar2<CR>')
Map('x', 'f', '<CMD>HopChar2<CR>')
Map('n', '<C-J>', ':HopLine<CR>')

-- EasyAlign
Map("v", "<leader>e", ":EasyAlign<CR>")

-- vim-go
Map('n', 'got', ':GoTestFunc<CR>')
Map('n', 'gor', ':GoRun<CR>')

-- nvim-tree
Map("n", "tt", ":NvimTreeToggle<CR>")
Map("n", "tr", ":NvimTreeRefresh<CR>")

Map("n", "<C-\\>", [[:FTermToggle<CR>]])
Map("t", "<C-\\>", [[<C-\><C-n>:FTermToggle<CR>]])
Map("t", "<C-n>", [[<C-\><C-n>]])
