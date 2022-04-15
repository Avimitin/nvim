local g = vim.g

if not vim.fn.has("nvim-0.6") then
  -- for filetype.nvim
  -- If using a Neovim version earlier than 0.6.0
  g.did_load_filetypes = 1
end

-- for vsnip
g.vsnip_snippet_dir = vim.fn.expand("~/.config/nvim/vsnip")

-- for wildfire
g.wildfire_objects = { "i'", 'i"', "i)", "i]", "i}", "ip", "it", "i`" }

-- for vim-markdown
g.vim_markdown_conceal_code_blocks = 0
g.vim_markdown_strikethrough = 1
g.vim_markdown_math = 1

g.rooter_manual_only = 1
g.rooter_change_directory_for_non_project_files = "current"
g.rooter_patterns = {
  ".git",
  "Cargo.toml",
  "package.json",
  "tsconfig.json",
}

-- enable treesitter for what filetype?
g.enable_treesitter_ft = {
  "c",
  "comment",
  "cpp",
  "fish",
  "go",
  "javascript",
  "json",
  "lua",
  "nix",
  "rust",
  "toml",
}

-- enable lspconfig for what filetype?
g.enable_lspconfig_ft = {
  "bash",
  "c",
  "cpp",
  "go",
  "html",
  "javascript",
  "json",
  "lua",
  "python",
  "sh",
  "toml",
}
