require'nvim-treesitter.configs'.setup {
	ensure_installed = {
		"cpp",
		"toml",
		"rust",
		"go",
		"json",
		"lua",
		"comment",
		"fish",
	},
	highlight = {
		enable = true,
	}
}

require('FTerm').setup {
	dimensions = {
		height = 0.9,
		width = 0.9,
	},

	border = 'single'
}

require("bufferline").setup{
	options = {
		indicator_icon = ' ',
		modified_icon = '',
	}
}
