local nmap = require("editor.utils").nmap

-- fugitive
-- keep the same prefix as the git sign
-- See git-sign keymap in lua/plugins/config/gitsign_cfg.lua
nmap("gic", ":Git commit -sS<CR>")
nmap("giP", ":Dispatch! git push ", { silent = false })
nmap("<leader>g", [[<CMD>Git<CR>]])
