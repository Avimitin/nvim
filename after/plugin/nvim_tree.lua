if require("libs.g")["nvim_tree"] then
  return
end

require("libs.keymaps").nmap("<leader>t", ":NvimTreeToggle<CR>")
