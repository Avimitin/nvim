local pack = require("pack").register

pack("iamcco/markdown-preview.nvim", {
  build = function()
    local ui = vim.api.nvim_list_uis()
    if ui and #ui > 0 then
      vim.fn["mkdp#util#install"]()
    end
  end,
  ft = "markdown",
  init = function()
    -- I don't know why markdown-preview doesn't want a vim.g.func = lua func
    vim.cmd([[
      function! g:MkdpBrowserFunc(url)
        :echomsg a:url
      endfunction
    ]])
    vim.g.mkdp_browserfunc = "g:MkdpBrowserFunc"
    vim.g.mkdp_open_to_the_world = 1
    vim.g.mkdp_port = "57843"
  end,
})

pack("dhruvasagar/vim-table-mode", {
  cmd = "TableModeToggle",
})

pack("lukas-reineke/headlines.nvim", {
  ft = { "org", "markdown" },
  config = function()
    require("headlines").setup({
      markdown = {
        headline_highlights = { "Headline1", "Headline2", "Headline3" },
      },
      org = {
        headline_highlights = { "Headline1", "Headline2", "Headline3" },
      },
    })
  end,
})
