if require("libs.g")("multi_cursor") then
  return
end

-- visual multi mappings
-- clean the keymap `u` and initialize the new keymap set
require("libs.keymaps").map("", "u", "<nop>")

-- u is map to <C-z>, let us reuse it here
vim.g.VM_maps = {
  ["Find Under"] = "un",
  ["Find Subword Under"] = "un",
  ["Select Cursor Down"] = "<C-down>",
  ["Select Cursor Up"] = "<C-up>",
  ["Select All"] = "uA",
  ["Undo"] = "<C-z>",
  ["Redo"] = "<C-r>",
  ["Start Regex Search"] = "ux",
  ["Visual Regex"] = "ux",
  ["Visual All"] = "uA",
  ["Visual Add"] = "ua",
  ["Visual Find"] = "uf",
  ["Visual Cursors"] = "uc",
}
