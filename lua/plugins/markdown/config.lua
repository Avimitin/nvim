local M = {
  pre = function()
    -- markdown preview
    vim.g.mkdp_browser = "surf"
    vim.g.mkdp_open_to_the_world = 1
    vim.g.mkdp_port = "57843"

    -- for vim-markdown
    vim.g.vim_markdown_conceal_code_blocks = 0
    vim.g.vim_markdown_strikethrough = 1
    vim.g.vim_markdown_math = 1
  end,
  post = nil,
}

return M
