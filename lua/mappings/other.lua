local map = require("mappings.utils").map
local nmap = require("mappings.utils").nmap

-- EasyAlign
map("v", "<leader>e", ":EasyAlign<CR>")

-- nvim-tree
nmap(";t", ":NvimTreeToggle<CR>")

-- fterm
nmap("<C-\\>", [[:ToggleTerm direction=float<CR>]])
nmap("<M-`>", [[:ToggleTerm direction=horizontal<CR>]])
map("t", "<C-\\>", [[<C-\><C-n>:ToggleTerm<CR>]])
map("t", "<C-n>", [[<C-\><C-n>]])
-- This for horizontal terminal
map("t", ";k", [[<C-\><C-n><C-w>k]])
-- This for vertical terminal
map("t", ";h", [[<C-\><C-n><C-w>h]])

-- telescope
nmap(";f", function()
  require("telescope.builtin").find_files()
end)
nmap(";s", function()
  require("telescope.builtin").live_grep()
end)
nmap("<leader>t", function()
  require("telescope.command").load_command(nil, nil, nil, "builtin", "theme=ivy")
end)

-- fugitive
nmap(";g", [[<CMD>Git<CR>]])

-- lazygit
nmap("<LEADER>l", [[<CMD>LazyGit<CR>]])

-- bufferline tab stuff
nmap("<C-c>", ":BufferLinePickClose<CR>") -- close tab

-- move between tabs
nmap(";n", [[<Cmd>BufferLineCycleNext<CR>]])
nmap(";p", [[<Cmd>BufferLineCyclePrev<CR>]])

-- move tabs
nmap("<A->>", [[<CMD>BufferLineMoveNext<CR>]])
nmap("<A-<>", [[<CMD>BufferLineMovePrev<CR>]])
nmap("<A-p>", [[<CMD>:BufferLinePick<CR>]])

-- dispatch
nmap(";d", ":Dispatch ", { noremap = true, silent = false })

-- fugitive
-- keep the same prefix as the git sign
-- See git-sign keymap in lua/plugins/config/gitsign_cfg.lua
nmap("gic", ":Git commit -sS<CR>")
nmap("giP", ":Git! push ", { silent = false })

-- kill buffer with ;q , quit window with :q . This make sense.
nmap(";q", require("plugins.bufdel").delete_buffer)
