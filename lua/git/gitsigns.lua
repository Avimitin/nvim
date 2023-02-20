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
      text = "▎",
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
      hl = "GitSignsDelete",
      text = "▎",
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
    map("n", "]g", function()
      if vim.wo.diff then
        return "]g"
      end
      vim.schedule(function()
        gs.next_hunk()
      end)
      return "<Ignore>"
    end, { expr = true, desc = "Go to next hunk" })

    map("n", "[g", function()
      if vim.wo.diff then
        return "[g"
      end
      vim.schedule(function()
        gs.prev_hunk()
      end)
      return "<Ignore>"
    end, { expr = true, desc = "Go to previous hunk" })

    -- Actions
    map({ "n", "v" }, "<leader>gs", ":Gitsigns stage_hunk<CR>")
    map({ "n", "v" }, "<leader>gr", ":Gitsigns reset_hunk<CR>")
    map("n", "<leader>gS", gs.stage_buffer)
    map("n", "<leader>gu", gs.undo_stage_hunk)
    map("n", "<leader>gR", gs.reset_buffer)
    map("n", "<leader>gp", gs.preview_hunk)
    map("n", "<leader>gb", function()
      gs.blame_line({ full = true })
    end)
    map("n", "<leader>gb", gs.toggle_current_line_blame)
    map("n", "<leader>gd", gs.diffthis)
    map("n", "<leader>gD", function()
      gs.diffthis("~")
    end)
    map("n", "<leader>gt", gs.toggle_deleted)

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
