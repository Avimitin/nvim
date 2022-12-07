local ok, Hydra = pcall(require, "hydra")
if not ok then
  return
end
local cmd = require("hydra.keymap-util").cmd
local pcmd = require("hydra.keymap-util").pcmd

local window_hint = [[
 ^^^^^^^^^^        Move      ^^           Size   ^^   ^^          Split
 ^^^^^^^^^^   --------------  ^^     --------------^^   ^^  ---------------
  ^^ ^    ^ ^    ^^^ ^   ^ ^
   ^ ^ _k_ ^ ^      ^ ^ _K_ ^ ^   ^       _<Up>_   ^       _s_:  horizontally 
  _h_ ^ ^ _l_    _H_ ^ ^ _L_   _<Left>_    _<Right>_   _v_:  vertically
   ^ ^ _j_ ^ ^      ^ ^ _J_ ^ ^   ^      _<Down>_   ^      _c_:  close
  ^^ ^    ^ ^    ^^^ ^  
   focus^^^^^^      window
 ^ ^ ^ ^ ^ ^  ^ ^ ^ ^ ^ ^   ^^ ^    ^^        _=_: equalize  _o_: remain only
 _q_/_<Esc>_: Quit
]]

Hydra({
  name = "Windows",
  hint = window_hint,
  config = {
    invoke_on_body = true,
    hint = {
      border = "rounded",
      offset = -1,
    },
  },
  mode = "n",
  body = "<C-w>",
  heads = {
    { "h", "<C-w>h" },
    { "j", "<C-w>j" },
    { "k", pcmd("wincmd k", "E11", "close") },
    { "l", "<C-w>l" },

    { "H", cmd("WinShift left") },
    { "J", cmd("WinShift down") },
    { "K", cmd("WinShift up") },
    { "L", cmd("WinShift right") },

    { "<Down>", cmd("res -5") },
    { "<Up>", cmd("res +5") },
    { "<Left>", cmd("vertical resize -5") },
    { "<Right>", cmd("vertical resize +5") },
    { "=", "<C-w>=", { desc = "equalize" } },

    { "s", pcmd("split", "E36") },
    { "<C-s>", pcmd("split", "E36"), { desc = false } },
    { "v", pcmd("vsplit", "E36") },
    { "<C-v>", pcmd("vsplit", "E36"), { desc = false } },

    { "w", "<C-w>w", { exit = true, desc = false } },
    { "<C-w>", "<C-w>w", { exit = true, desc = false } },

    { "o", "<C-w>o", { exit = true, desc = "remain only" } },
    { "<C-o>", "<C-w>o", { exit = true, desc = false } },

    { "c", pcmd("close", "E444") },
    { "<C-c>", pcmd("close", "E444"), { desc = false } },

    { "<Esc>", nil, { exit = true, desc = false } },
    { "q", nil, { exit = true, desc = "close window" } },
  },
})
