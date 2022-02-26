local map = require("core.utils").map
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

map("n", ";d", ":Dispatch ", { noremap = true, silent = false })
