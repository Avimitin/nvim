-- rename this file to custom.lua to configure the nvim

local my_config = {
  -- the global theme settings
  theme = "kanagawa",

  -- Single string or a array with one item represent filetype that
  -- nvim-treesitter should load.
  --
  -- Array with two item which sencond item is lsp server means both
  -- lspconfig and nvim-treesitter should be load on this filetype,
  -- and lspconfig will automatically install the server.
  --
  -- WARNING: Please do not set rust-analyzer here. The rust-tools.nvim plugin
  -- is already handle the lsp server.
  langs = {
    "bash",
    "comment",
    "fish",
    "html",
    "json",
    "nix",
    "rust",
    "toml",
    { "vim" },
    { "go", "gopls" },
    { "lua", "sumneko_lua" },
    { "c", "clangd" }, -- require npm
    { "cpp", "clangd" }, -- require npm
    { "javascript", "eslint" }, -- require npm
    { "python", "pyright" }, -- require npm
  },

  -- configuration for null-ls lsp injection
  null_ls = {
    enable_stylua_fmt = false, -- require stylua executable
  },

  autocmd_enable = {
    fcitx5 = false, -- require fcitx5-remote
    lastline = false,
    diff_on_commit = false, -- might mess up your window
  },

  markdown = {
    -- must be executable
    preview_browser = "chrome",
  },
}

return my_config
