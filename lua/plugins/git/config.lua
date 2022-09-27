local config = {}

config.gitsigns_config = function()
  require("gitsigns").setup({
    signs = {
      add = {
        hl = "GitSignsAdd",
        text = "▎",
        numhl = "GitSignsAddNr",
        linehl = "GitSignsAddLn",
      },
      change = {
        hl = "GitSignsChange",
        text = "░",
        numhl = "GitSignsChangeNr",
        linehl = "GitSignsChangeLn",
      },
      delete = {
        hl = "GitSignsDelete",
        text = "_",
        numhl = "GitSignsDeleteNr",
        linehl = "GitSignsDeleteLn",
      },
      topdelete = {
        hl = "GitSignsDelete",
        text = "‾",
        numhl = "GitSignsDeleteNr",
        linehl = "GitSignsDeleteLn",
      },
      changedelete = {
        hl = "GitSignsChange",
        text = "▒",
        numhl = "GitSignsChangeNr",
        linehl = "GitSignsChangeLn",
      },
    },
    on_attach = function(bufnr)
      local gs = package.loaded.gitsigns

      local function map(mode, l, r, opts)
        opts = opts or {}
        opts.buffer = bufnr
        vim.keymap.set(mode, l, r, opts)
      end

      -- Navigation
      map("n", "gij", function()
        if vim.wo.diff then
          return "]c"
        end
        vim.schedule(function()
          gs.next_hunk()
        end)
        return "<Ignore>"
      end, { expr = true })

      map("n", "gik", function()
        if vim.wo.diff then
          return "[c"
        end
        vim.schedule(function()
          gs.prev_hunk()
        end)
        return "<Ignore>"
      end, { expr = true })

      -- Actions
      map({ "n", "v" }, "gis", ":Gitsigns stage_hunk<CR>")
      map({ "n", "v" }, "gir", ":Gitsigns reset_hunk<CR>")
      map("n", "giS", gs.stage_buffer)
      map("n", "giu", gs.undo_stage_hunk)
      map("n", "giR", gs.reset_buffer)
      map("n", "gip", gs.preview_hunk)
      map("n", "giB", function()
        gs.blame_line({ full = true })
      end)
      map("n", "gib", gs.toggle_current_line_blame)
      map("n", "gid", gs.diffthis)
      map("n", "giD", function()
        gs.diffthis("~")
      end)

      -- Text object
      map({ "o", "x" }, "ih", ":<C-U>Gitsigns select_hunk<CR>")
    end,
    numhl = true,
    linehl = false,
    watch_gitdir = { interval = 1000, follow_files = true },
    current_line_blame = false,
    current_line_blame_opts = {
      virt_text = true,
      virt_text_pos = "eol", -- 'eol' | 'overlay' | 'right_align'
      delay = 1000,
    },
    sign_priority = 6,
    update_debounce = 100,
    status_formatter = nil, -- Use default
    word_diff = false,
    diff_opts = { internal = true },
  })
end

return config
