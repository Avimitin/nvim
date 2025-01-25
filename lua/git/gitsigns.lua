require("gitsigns").setup({
  signs = {
    add = {
      text = "▐",
    },
    change = {
      text = "▐",
    },
    untracked = {
      text = "▐",
    },
    delete = {
      text = "▐",
    },
    topdelete = {
      text = "󱨉",
    },
    changedelete = {
      text = "▐",
    },
  },
  on_attach = function(bufnr)
    local gs = package.loaded.gitsigns

    require("builder.key-mapper").bufmap(bufnr, "n", {
      { "<leader>gS", gs.stage_buffer, desc = "Stage buffer" },
      { "<leader>gu", gs.undo_stage_hunk, desc = "Undo stage hunk" },
      { "<leader>gR", gs.reset_buffer, desc = "Reset buffer" },
      { "<leader>gp", gs.preview_hunk, desc = "Preview hunk" },
      { "<leader>gq", gs.setqflist, desc = "Set qflist for hunk in current buffer" },
      {
        "<leader>gQ",
        function()
          gs.setqflist("all")
        end,
        desc = "Set qflist for all hunk",
      },
      {
        "<leader>gB",
        function()
          gs.blame_line({ full = true })
        end,
        desc = "Open git blame panel",
      },
      { "<leader>gb", gs.toggle_current_line_blame, desc = "Enter line blame mode" },
      { "<leader>gD", gs.diffthis, desc = "Open diff" },
      { "<leader>gd", gs.toggle_deleted, desc = "Toggle deleted line" },
      {
        "]g",
        function()
          if vim.wo.diff then
            return "]g"
          end
          vim.schedule(function()
            gs.next_hunk()
          end)
          return "<Ignore>"
        end,
        expr = true,
        desc = "Go to next hunk",
      },
      {
        "[g",
        function()
          if vim.wo.diff then
            return "[g"
          end
          vim.schedule(function()
            gs.prev_hunk()
          end)
          return "<Ignore>"
        end,
        expr = true,
        desc = "Go to previous hunk",
      },
    })

    -- Actions
    require("builder.key-mapper").bufmap(bufnr, { "n", "v" }, {
      { "<leader>gs", ":Gitsigns stage_hunk<CR>", desc = "Stage hunk" },
      { "<leader>gr", ":Gitsigns reset_hunk<CR>", desc = "Reset hunk" },
    })

    -- Text object
    require("builder.key-mapper").bufmap(
      bufnr,
      { "o", "x" },
      { "ih", ":<C-U>Gitsigns select_hunk<CR>", desc = "Select hunk" }
    )
  end,
  numhl = false,
  linehl = false,
  watch_gitdir = { interval = 1000, follow_files = true },
  current_line_blame = false,
  current_line_blame_opts = {
    virt_text = true,
    virt_text_pos = "eol", -- 'eol' | 'overlay' | 'right_align'
    delay = 1000,
  },
  sign_priority = 2000,
  update_debounce = 100,
  status_formatter = nil, -- Use default
  word_diff = false,
  diff_opts = { internal = true },
})
