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
  Map("n", "<C-\\>", [[<CMD>lua require("FTerm").toggle()<CR>]], {})
  Map("t", "<C-\\>", [[<C-\><C-n><CMD>lua require("FTerm").toggle()<CR>]], {})
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
else
	print("load bufferling...failed")
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
-- vim-go settings
--===============================================]]
g.go_echo_go_info = 0
g.go_doc_popup_window = 1
g.go_def_mapping_enabled = 0
g.go_template_autocreate = 0
g.go_textobj_enabled = 0
g.go_auto_type_info = 1
g.go_def_mapping_enabled = 0
g.go_highlight_array_whitespace_error = 1
g.go_highlight_build_constraints = 1
g.go_highlight_chan_whitespace_error = 1
g.go_highlight_extra_types = 1
g.go_highlight_fields = 1
g.go_highlight_format_strings = 1
g.go_highlight_function_calls = 1
g.go_highlight_function_parameters = 1
g.go_highlight_functions = 1
g.go_highlight_generate_tags = 1
g.go_highlight_methods = 1
g.go_highlight_operators = 1
g.go_highlight_space_tab_error = 1
g.go_highlight_string_spellcheck = 1
g.go_highlight_structs = 1
g.go_highlight_trailing_whitespace_error = 1
g.go_highlight_types = 1
g.go_highlight_variable_assignments = 0
g.go_highlight_variable_declarations = 0
g.go_doc_keywordprg_enabled = 0

-- gitsign settings
pcall(require, 'plugins.gitsign')

--anyjump
g.any_jump_window_width_ratio  = 0.8
g.any_jump_window_height_ratio = 0.9
g.any_jump_disable_default_keybindings = 1

--markdown preview
g.mkdp_browser = 'firefox'
g.mkdp_open_to_the_world = 1
g.mkdp_port = '57843'

--easymotion
g.EasyMotion_do_mapping = 0
g["nnn#layout"] = {
	left='~20%',
	window= {
		width= 0.8,
		height= 0.6,
		highlight='Debug'
	}
}

-- hop: vim-motion
require'hop'.setup {
	keys = 'etovxqpdygfblzhckisuran'
}

-- nvim-autopairs: bracket pairing
require('nvim-autopairs').setup()
