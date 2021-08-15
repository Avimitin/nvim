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
Map("n",  ";",     ":")

vim.g.mapleader=" "

Map("n", ";w",        ":w<CR>")

Map("n", "<LEADER>q", ":wq<CR>")

Map("n", "<C-A-q>",     ":qa<CR>")

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

--center line
Map("i", "<C-c>",     "<ESC>zzi")

--nnn
Map('n', '<Leader>o', ':NnnPicker %:p:h<CR>')

--hop
Map('n', 'f'        , ':HopChar2<CR>')
Map('v', 'f'        , ':HopChar2<CR>')
Map('n', '<Leader>j', ':HopLine<CR>')
Map('n', '<Leader>k', ':HopLine<CR>')

--telescope
Map('n', '<leader>ff', [[<cmd>Telescope find_files<cr>]])
Map('n', '<leader>fp', [[<cmd>Telescope media_files<cr>]])
Map('n', '<leader>fg', [[<cmd>Telescope live_grep<cr>]])
Map('n', '<leader>fb', [[<cmd>Telescope buffers<cr>]])
Map('n', '<leader>fh', [[<cmd>Telescope help_tags<cr>]])

--SymbolsOutline
Map("n", "<leader>l", ":SymbolsOutline<CR>")

--EasyAlign
Map("v", "<leader>e", ":EasyAlign<CR>")

--AnyJump
Map('n', '<leader>aj', ':AnyJump<CR>')
Map('n', '<leader>ab', ':AnyJumpBack<CR>')

--vim-go
Map('n', 'got', ':GoTestFunc<CR>')
Map('n', 'gor', ':GoRun<CR>')

--nvim-tree
Map("n", "tt", ":NvimTreeToggle<CR>")
Map("n", "tr", ":NvimTreeRefresh<CR>")
