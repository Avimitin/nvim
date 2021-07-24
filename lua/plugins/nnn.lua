vim.g["nnn#layout"] = {
	left='~20%',
	window= {
		width= 0.7,
		height= 0.8,
		highlight='Debug'
	}
}

vim.api.nvim_set_keymap('n', '<Leader>o', ':NnnPicker %:p:h<CR>', {noremap=true, silent=true})
