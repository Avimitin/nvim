if require("libs.g").cinnamon then
  return
end

local map = require("libs.keymaps").map
local function scroll(a1, a2, a3, a4)
  return function()
    require("cinnamon.scroll").scroll(a1, a2, a3, a4)
  end
end

map({ "n", "x" }, "J", scroll("5j", 1, 1))
map({ "n", "x" }, "K", scroll("5k", 1, 1))
map({ "n", "x" }, "gg", scroll("gg", 1))
map({ "n", "x" }, "G", scroll("G", 1, 1))
