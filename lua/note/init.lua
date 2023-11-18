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
    --vim.g.mkdp_browser = vim.cfg.markdown.previewer
    vim.cmd([[
      function! g:MkdpBrowserFunc(url)
        :echo a:url
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

pack("nvim-orgmode/orgmode", {
  ft = "org",
  keys = {
    "<leader>oa",
    "<leader>oc",
  },
  config = function()
    require("orgmode").setup_ts_grammar()
    require("orgmode").setup({
      org_agenda_files = { "~/todo.org", "~/me/notes/daily-report/org/**/*" },
      org_default_notes_file = "~/me/notes/refile.org",
      org_archive_location = "~/.cache/org-mode/%s_archive",
      org_todo_keywords = { "TODO(t)", "BLOCK(b)", "|", "DONE(d)" },
      org_todo_keyword_faces = {
        TODO = ":background #43242B :weight bold",
        BLOCK = ":background #49443C :foreground #DCA561 :weight bold",
        DONE = ":background #2B3328 :weight bold",
      },
    })
    vim.api.nvim_create_autocmd("FileType", {
      pattern = "org",
      callback = function()
        vim.wo.conceallevel = 2
        vim.wo.concealcursor = "nc"
      end,
    })
  end,
})

pack("akinsho/org-bullets.nvim", {
  ft = { "org" },
  config = function()
    require("org-bullets").setup()
  end,
})
