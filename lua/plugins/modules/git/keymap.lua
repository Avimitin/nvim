local nmap = require("mappings.utils").nmap

-- fugitive
-- keep the same prefix as the git sign
-- See git-sign keymap in lua/plugins/config/gitsign_cfg.lua
nmap("gic", ":Git commit -sS<CR>")
nmap("giP", ":Dispatch! git push ", { silent = false })
nmap(";g", [[<CMD>Git<CR>]])

-- lazygit
nmap("<LEADER>l", [[<CMD>LazyGit<CR>]])
