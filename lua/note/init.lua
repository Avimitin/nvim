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
        bullets = { "❑", "◉", "◎", "○" },
        headline_highlights = { "Headline1", "Headline2", "Headline3" },
      },
      org = {
        headline_highlights = { "Headline1", "Headline2", "Headline3" },
      },
    })
  end,
})

pack("epwalsh/obsidian.nvim", {
  version = "*", -- recommended, use latest release instead of latest commit
  lazy = true,
  ft = "markdown",
  cmd = { "ObsidianNew", "ObsidianSwitch", "ObsidianToday" },
  config = function()
    require("obsidian").setup({
      workspaces = {
        {
          name = "notes",
          path = "~/me/notes",
        },
      },
      mappings = {
        ["<leader>gf"] = {
          action = function()
            return require("obsidian").util.gf_passthrough()
          end,
          opts = { noremap = false, expr = true, buffer = true },
        },
        ["<leader>ch"] = {
          action = function()
            return require("obsidian").util.toggle_checkbox()
          end,
          opts = { buffer = true },
        },
      },

      note_id_func = function(title)
        -- Create note IDs in a Zettelkasten format with a timestamp and a suffix.
        -- In this case a note with the title 'My new note' will be given an ID that looks
        -- like '1657296016-my-new-note', and therefore the file name '1657296016-my-new-note.md'
        if title ~= nil then
          -- If title is given, transform it into valid file name.
          return title:gsub(" ", "-"):gsub("[^A-Za-z0-9-]", ""):lower()
        end

        local suffix = ""
        -- If title is nil, just add 4 random uppercase letters to the suffix.
        for _ = 1, 4 do
          suffix = suffix .. string.char(math.random(65, 90))
        end
        return tostring(os.time()) .. "-" .. suffix
      end,
    })
  end,
})
