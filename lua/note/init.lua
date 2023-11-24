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

pack("nvim-orgmode/orgmode", {
  ft = "org",
  keys = {
    {
      "<leader>oa",
      function()
        require("orgmode").action("agenda.prompt")
      end,
      desc = "[orgmode] Prompt Agenda",
    },
    {
      "<leader>oc",
      function()
        require("orgmode").action("capture.prompt")
      end,
      desc = "[orgmode] Prompt capture",
    },
  },
  config = function()
    require("orgmode").setup_ts_grammar()
    require("orgmode").setup({
      org_agenda_files = { "~/todo/**/*" },
      org_default_notes_file = "~/todo/todo.org",
      org_archive_location = "archives/%s_archive::",
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
