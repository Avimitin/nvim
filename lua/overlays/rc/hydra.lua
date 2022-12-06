local ok, Hydra = pcall(require, "hydra")
if not ok then
  return
end
local cmd = require("hydra.keymap-util").cmd
local window_hint = [[
                   WINDOWS OPERATION

    _h_:  Left | _j_:  Down  _k_:  Up | _l_:  Right

  _<Up>_:     Increse Height  _<Down>_:  Decrese Height
  _<Right>_:  Increase Width  _<Left>_:  Decrese Width

    _v_:  Horizontal Split      _x_:  Vertical Split
]]

Hydra({
  name = "Windows Operation",
  hint = window_hint,
  config = {
    color = "pink",
    invoke_on_body = "true",
    hint = {
      border = "rounded",
      position = "bottom",
    },
  },
  mode = "n",
  body = "<C-W>",
  heads = {
    { "h", "<C-W>h" },
    { "j", "<C-W>j" },
    { "k", "<C-W>k" },
    { "l", "<C-W>l" },
    { "v", "<C-W>v" },
    { "x", "<C-W>x" },
    { "<Down>", cmd("res -5") },
    { "<Up>", cmd("res +5") },
    { "<Left>", cmd("vertical resize -5") },
    { "<Right>", cmd("vertical resize +5") },
  },
})
