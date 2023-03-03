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

pack("nvim-orgmode/orgmode", {
  build = ":TSUpdate org",
  ft = "org",
  keys = {
    "<leader>oa",
    "<leader>oc",
  },
  config = function()
    require("orgmode").setup_ts_grammar()
    require("orgmode").setup({
      org_agenda_files = { "~/Documents/schedule/*" },
      org_default_notes_file = "~/Documents/schedule/todo.org",
      org_todo_keywords = { "TODO(t)", "TRACKING(p)", "SOMEDAY(s)", "|", "DONE(d)" },
      mappings = {
        org = {
          org_toggle_checkbox = "<leader><space>",
        },
      },
    })

    require("cmp").setup.filetype("org", {
      sources = {
        { name = "orgmode" },
        { name = "path" },
        { name = "buffer" },
      },
    })
  end,
})
