if require("libs.g").toggleterm then
  return
end
local map = require("libs.keymaps").map
local nmap = require("libs.keymaps").nmap
-- float terminal
nmap("<C-\\>", [[:ToggleTerm direction=float<CR>]])
map("t", "<C-\\>", [[<C-\><C-n>:ToggleTerm<CR>]])
-- horizontal terminal
nmap("_", [[:ToggleTerm direction=horizontal<CR>]])
map("t", "<A-;>", [[<C-\><C-n>]])
-- terminal windows movement
map("t", "<C-k>", [[<C-\><C-n><C-w>k]])
map("t", "<C-l>", [[<C-\><C-n><C-w>l]])
map("t", "<C-h>", [[<C-\><C-n><C-w>h]])
