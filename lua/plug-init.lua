local g = vim.g

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
