local g = vim.g

--[[==============================================
-- treesitter settings
--===============================================]]
pcall(require, "plugins.treesitter")

--[[==============================================
-- fterm settings
--===============================================]]
local fterm_status = pcall(require, "plugins.fterm")
if fterm_status then
  Map("n", "<C-\\>", [[<CMD>lua require("FTerm").open()<CR>]], {})
end

--[[==============================================
-- bufferline settings
--===============================================]]
local bfl_status = pcall(require, "plugins.bufferline")
if bfl_status then
  local opt = {}
  -- bufferline tab stuff
  Map("n", "<S-t>", ":tabnew<CR>", opt) -- new tab
  Map("n", "<S-x>", ":BufferLinePickClose<CR>", opt) -- close tab

  -- move between tabs
  Map("n", ".", [[<Cmd>BufferLineCycleNext<CR>]], opt)
  Map("n", ",", [[<Cmd>BufferLineCyclePrev<CR>]], opt)

	-- move tabs
	Map("n", "<A->>", [[<CMD>BufferLineMoveNext<CR>]], opt)
	Map("n", "<A-<>", [[<CMD>BufferLineMovePrev<CR>]], opt)
end

--[[==============================================
-- nvim-tree file explorer settings
--===============================================]]
local tree_stat = pcall(require, "plugins.nvimtree")
if tree_stat then
  Map("n", "tt", ":NvimTreeToggle<CR>", {})
end

--[[==============================================
-- lsp settings
--===============================================]]
pcall(require, "plugins.lsp")

--[[==============================================
-- compe settings
--===============================================]]
local compe_stat = pcall(require, "plugins.compe")
if compe_stat then
	SetCompleteKey()
end

--[[==============================================
-- telescope settings
--===============================================]]
local ts_stat = pcall(require, "plugins.telescope")
if ts_stat then
  Map('n', '<leader>ff', [[<cmd>Telescope find_files<cr>]], {})
  Map('n', '<leader>fg', [[<cmd>Telescope live_grep<cr>]], {})
  Map('n', '<leader>fb', [[<cmd>Telescope buffers<cr>]], {})
  Map('n', '<leader>fh', [[<cmd>Telescope help_tags<cr>]], {})
end

--[[==============================================
-- indent settings
--===============================================]]
require("plugins.indent")

--[[==============================================
-- gitgutter settings
--===============================================]]
g.gitgutter_sign_allow_clobber = 0
g.gitgutter_map_keys = 0
g.gitgutter_override_sign_column_highlight = 0
g.gitgutter_preview_win_floating = 1
g.gitgutter_sign_added = '▎'
g.gitgutter_sign_modified = '░'
g.gitgutter_sign_removed = '▏'
g.gitgutter_sign_removed_first_line = '▔'
g.gitgutter_sign_modified_removed = '▒'
Map('n', 'giu', ':GitGutterUndoHunk<CR>')
Map('n', 'gis', ':GitGutterStageHunk<CR>')
Map('n', 'gip', ':GitGutterPreviewHunk<CR>')
Map('n', 'gi=', ':GitGutterNextHunk<CR>')
Map('n', 'gi-', ':GitGutterPrevHunk<CR>')
