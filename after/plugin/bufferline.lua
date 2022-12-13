if require("libs.g").bufferline then
  return
end

local nmap = require("libs.keymaps").nmap
nmap("<C-c>", ":BufferLinePickClose<CR>") -- close tab
-- move between tabs
nmap("<Tab>", "<CMD>BufferLineCycleNext<CR>")
nmap("<S-Tab>", [[<Cmd>BufferLineCyclePrev<CR>]])

nmap("<leader>p", [[<CMD>:BufferLinePick<CR>]])
-- move tabs
nmap("<M-S-right>", [[<CMD>BufferLineMoveNext<CR>]])
nmap("<M-S-left>", [[<CMD>BufferLineMovePrev<CR>]])
