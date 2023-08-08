local keymapper = require("libs.keymap")

local function setup(options)
  keymapper.map("n", options.keymap.normal)
  keymapper.map("x", options.keymap.select)
  keymapper.map("i", options.keymap.insert)
  keymapper.map("t", options.keymap.terminal)
  vim.g.mapleader = options.keymap.mapleader
end

return {
  setup = setup
}
