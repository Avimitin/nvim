require'nvim-treesitter.configs'.setup {
	ensure_installed = {
		"cpp",
		"toml",
		"rust",
	},
	highlight = {
		enable = true,
	}
}
