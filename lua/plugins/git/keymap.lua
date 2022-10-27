local nmap = require("editor.utils").nmap

-- fugitive
-- keep the same prefix as the git sign
-- See git-sign keymap in lua/plugins/config/gitsign_cfg.lua
nmap("gic", ":Git commit -sS<CR>")
nmap("giP", ":Dispatch! git push ", { silent = false })
nmap("<leader>g", [[<CMD>Git<CR>]])

nmap("<leader>a", function()
  local present, opt = pcall(require, "custom")
  if not present or not opt.enable_lazygit then
    vim.notify(
      "You need to set `enable_lazygit` options to true in your lua/custom.lua file to enable this key mappings. ALERT: Download neovim-remote before you use this feature",
      vim.log.levels.ERROR
    )
    return
  end
  require("plugins.git.lazygit"):toggle()
end)
