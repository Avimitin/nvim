if require("libs.g").easyalign then
  return
end

require("libs.keymaps").map("v", "<space>e", ":EasyAlign<CR>")
