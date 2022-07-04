local present, custom = pcall(require, "custom")

local g = vim.g

if not vim.fn.has("nvim-0.6") then
  -- for filetype.nvim
  -- If using a Neovim version earlier than 0.6.0
  g.did_load_filetypes = 1
end

-- for vale
g.enable_vale = 0 -- set it to 1 after you setup vale

if present and custom.enable_vale then
  g.enable_vale = 1
end

-- for vsnip
g.vsnip_snippet_dir = vim.fn.expand("~/.config/nvim/vsnip")

-- for wildfire
g.wildfire_objects = { "i'", 'i"', "i)", "i]", "i}", "ip", "it", "i`" }

g.rooter_manual_only = 1
g.rooter_change_directory_for_non_project_files = "current"
g.rooter_patterns = {
  ".git",
  "Cargo.toml",
  "package.json",
  "tsconfig.json",
}

-- visual multi mappings
-- clean the keymap `u` and initialize the new keymap set
require("mappings.utils").map("", "u", "<nop>")
g.VM_default_mappings = 0

-- u is map to <C-z>, let us reuse it here
g.VM_maps = {
  ["Find Under"] = "un",
  ["Find Subword Under"] = "un",
  ["Select Cursor Down"] = "uj",
  ["Select Cursor Up"] = "uk",
  ["Undo"] = "<C-z>",
  ["Redo"] = "<C-r>",
  ["Start Regex Search"] = "ux",
  ["Visual Regex"] = "ux",
  ["Visual All"] = "uA",
  ["Visual Add"] = "ua",
  ["Visual Find"] = "uf",
  ["Visual Cursors"] = "uc",
}
