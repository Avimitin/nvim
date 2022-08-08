local M = {
  pre = function()
    local present, custom = pcall(require, "custom")
    if
      present
      and custom.markdown
      and custom.markdown.preview_browser
      and #custom.markdown.preview_browser > 0
    then
      vim.g.mkdp_browser = custom.markdown.preview_browser
    end

    vim.g.mkdp_browser = "firefox"
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
