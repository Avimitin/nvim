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
    local function map(mode, lhs, rhs, opts)
      opts = vim.tbl_extend("force", { noremap = true, silent = true }, opts or {})
      vim.api.nvim_buf_set_keymap(bufnr, mode, lhs, rhs, opts)
    end

    -- Navigation
    map("n", "gij", "&diff ? ']c' : '<cmd>Gitsigns next_hunk<CR>'", { expr = true })
    map("n", "gik", "&diff ? '[c' : '<cmd>Gitsigns prev_hunk<CR>'", { expr = true })

    -- Actions
    map("n", "gis", ":Gitsigns stage_hunk<CR>")
    map("v", "gis", ":Gitsigns stage_hunk<CR>")
    map("n", "gir", ":Gitsigns reset_hunk<CR>")
    map("v", "gir", ":Gitsigns reset_hunk<CR>")
    map("n", "giS", "<cmd>Gitsigns stage_buffer<CR>")
    map("n", "giu", "<cmd>Gitsigns undo_stage_hunk<CR>")
    map("n", "giR", "<cmd>Gitsigns reset_buffer<CR>")
    map("n", "gip", "<cmd>Gitsigns preview_hunk<CR>")
    map("n", "giB", '<cmd>lua require"gitsigns".blame_line{full=true}<CR>')
    map("n", "gib", "<cmd>Gitsigns toggle_current_line_blame<CR>")
    map("n", "gid", "<cmd>Gitsigns diffthis<CR>")
    map("n", "giD", '<cmd>lua require"gitsigns".diffthis("~")<CR>')
    map("n", "gih", "<cmd>Gitsigns toggle_deleted<CR>")

    -- Text object
    map("o", "ih", ":<C-U>Gitsigns select_hunk<CR>")
    map("x", "ih", ":<C-U>Gitsigns select_hunk<CR>")
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
