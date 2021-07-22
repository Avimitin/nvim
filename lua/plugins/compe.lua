local present, compe = pcall(require, "compe")
if not present then
    return
end

compe.setup {
    enabled = true,
    autocomplete = true,
    debug = false,
    min_length = 1,
    preselect = "enable",
    throttle_time = 80,
    source_timeout = 200,
    incomplete_delay = 400,
    max_abbr_width = 100,
    max_kind_width = 100,
    max_menu_width = 100,
    documentation = true,
    source = {
        buffer = {kind = "﬘", true},
        luasnip = {kind = "﬌", true},
        nvim_lsp = true,
        nvim_lua = true
    }
}

-- Utility functions for compe and luasnip
local t = function(str)
	return vim.api.nvim_replace_termcodes(str, true, true, true)
end

local check_back_space = function()
	local col = vim.fn.col '.' - 1
	return col == 0 or vim.fn.getline('.'):sub(col, col):match '%s' ~= nil
end

-- Use (s-)tab to:
--- move to prev/next item in completion menu
--- jump to prev/next snippet's placeholder
local snip_stat, luasnip = pcall(require, 'luasnip')

_G.tab_complete = function()
	if vim.fn.pumvisible() == 1 then
		return t '<C-n>'
	elseif snip_stat and luasnip.expand_or_jumpable() then
		return t '<Plug>luasnip-expand-or-jump'
	elseif check_back_space() then
		return t '<Tab>'
	else
		return vim.fn['compe#complete']()
	end
end

_G.s_tab_complete = function()
	if vim.fn.pumvisible() == 1 then
		return t '<C-p>'
	elseif snip_stat and luasnip.jumpable(-1) then
		return t '<Plug>luasnip-jump-prev'
	else
		return t '<S-Tab>'
	end
end

local options = { noremap=true, silent=true, expr = true}
-- Map tab to the above tab complete functions
vim.api.nvim_set_keymap('i', '<Tab>', 'v:lua.tab_complete()', options)
vim.api.nvim_set_keymap('s', '<Tab>', 'v:lua.tab_complete()', options)
vim.api.nvim_set_keymap('i', '<S-Tab>', 'v:lua.s_tab_complete()', options)
vim.api.nvim_set_keymap('s', '<S-Tab>', 'v:lua.s_tab_complete()', options)

-- Map compe confirm and complete functions
vim.api.nvim_set_keymap('i', '<cr>', 'compe#confirm("<cr>")', options)
vim.api.nvim_set_keymap('i', '<c-space>', 'compe#complete()', options)
