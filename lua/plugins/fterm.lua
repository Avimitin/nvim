if not pcall(require, 'FTerm') then
	print("Failed to call FTerm")
	return
end

require('FTerm').setup {
	dimensions = {
		height = 0.9,
		width = 0.9,
	},

	border = 'double'
}

local option = {noremap=true, silent=true}
vim.api.nvim_set_keymap("n", "<C-\\>", [[<CMD>lua require("FTerm").toggle()<CR>]], option)
vim.api.nvim_set_keymap("t", "<C-\\>", [[<C-\><C-n><CMD>lua require("FTerm").toggle()<CR>]], option)
