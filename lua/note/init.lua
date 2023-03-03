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
    -- Markdown Preview settings
    vim.g.mkdp_browser = vim.cfg.markdown.previewer
    vim.g.mkdp_open_to_the_world = 1
    vim.g.mkdp_port = "57843"
  end,
})

pack("dhruvasagar/vim-table-mode", {
  cmd = "TableModeToggle",
})

pack("nvim-neorg/neorg", {
  build = ":Neorg sync-parsers",
  ft = "norg",
  cmd = "Neorg",
  config = function()
    require("neorg").setup({
      load = {
        ["core.defaults"] = {},
        ["core.norg.concealer"] = {},
        ["core.norg.completion"] = {
          config = {
            engine = "nvim-cmp",
          },
        },
        ["core.norg.dirman"] = {
          config = {
            workspaces = {
              notes = "~/Documents/notes",
              work = "~/Documents/schedule",
            },
          },
        },
      },
    })

    require("cmp").setup.filetype("norg", {
      sources = {
        { name = "neorg" },
        { name = "path" },
        { name = "buffer" },
      },
    })
  end,
})
