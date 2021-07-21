local g = vim.g

pcall(require, "plugins.treesitter")

local fterm_status = pcall(require, "plugins.fterm")
if fterm_status then
  Map("n", "<C-\\>", [[<CMD>lua require("FTerm").open()<CR>]], {})
end

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

local tree_stat = pcall(require, "plugins.nvimtree")
if tree_stat then
  Map("n", "tt", ":NvimTreeToggle<CR>", {})
end

pcall(require, "plugins.lsp")

local compe_stat = pcall(require, "plugins.compe")
if compe_stat then
	SetCompleteKey()
end

local ts_stat = pcall(require, "plugins.telescope")
if ts_stat then
  Map('n', '<leader>ff', [[<cmd>Telescope find_files<cr>]], {})
  Map('n', '<leader>fg', [[<cmd>Telescope live_grep<cr>]], {})
  Map('n', '<leader>fb', [[<cmd>Telescope buffers<cr>]], {})
  Map('n', '<leader>fh', [[<cmd>Telescope help_tags<cr>]], {})
end

require("plugins.indent")

