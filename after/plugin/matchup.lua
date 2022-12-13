if require("libs.g").matchup then
  return
end

vim.g.matchup_matchparen_offscreen = {}
require("libs.keymaps").map({ "n", "x", "o" }, ",", "<Plug>(matchup-%)")
