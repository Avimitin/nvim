local setup_hydra = function(bufnr)
  local Hydra = require("hydra")
  local gitsigns = require("gitsigns")

  local hint = [[
   _j_: next hunk   _s_: stage hunk        _d_: show deleted   _b_: blame line
   _k_: prev hunk   _u_: undo last stage   _p_: preview hunk   _B_: blame show full 
   _D_: open diff   _S_: stage buffer      _r_: reset hunk     _R_: reset buffer
   ^ ^           _c_: git commit           _<Enter>_: GitStatus

   _q_: exit
   ]]

  Hydra({
    name = "Git",
    hint = hint,
    config = {
      buffer = bufnr,
      color = "pink",
      invoke_on_body = true,
      hint = {
        border = "rounded",
      },
      on_enter = function()
        vim.cmd("mkview")
        vim.cmd("silent! %foldopen!")
        vim.bo.modifiable = false
        gitsigns.toggle_linehl(true)
      end,
      on_exit = function()
        local cursor_pos = vim.api.nvim_win_get_cursor(0)
        vim.cmd("loadview")
        vim.api.nvim_win_set_cursor(0, cursor_pos)
        vim.cmd("normal zv")
        gitsigns.toggle_linehl(false)
        gitsigns.toggle_deleted(false)
      end,
    },
    mode = { "n", "x" },
    body = "<leader>g",
    heads = {
      {
        "j",
        function()
          if vim.wo.diff then
            return "]c"
          end
          vim.schedule(function()
            gitsigns.next_hunk()
          end)
          return "<Ignore>"
        end,
        { expr = true, desc = "next hunk" },
      },
      {
        "k",
        function()
          if vim.wo.diff then
            return "[c"
          end
          vim.schedule(function()
            gitsigns.prev_hunk()
          end)
          return "<Ignore>"
        end,
        { expr = true, desc = "prev hunk" },
      },
      { "s", ":Gitsigns stage_hunk<CR>", { silent = true, desc = "stage hunk" } },
      { "S", gitsigns.stage_buffer, { desc = "stage buffer" } },
      { "r", ":Gitsigns reset_hunk<CR>", { silent = true } },
      { "R", gitsigns.reset_buffer, { desc = "reset buffer", exit = true } },
      { "u", gitsigns.undo_stage_hunk, { desc = "undo last stage" } },
      { "p", gitsigns.preview_hunk, { desc = "preview hunk" } },
      { "d", gitsigns.toggle_deleted, { nowait = true, desc = "toggle deleted" } },
      { "D", gitsigns.diffthis, { nowait = true, desc = "toggle diff", exit = true } },
      { "b", gitsigns.blame_line, { desc = "blame" } },
      {
        "B",
        function()
          gitsigns.blame_line({ full = true })
        end,
        { desc = "blame show full" },
      },
      { "c", "<Cmd>Git commit -sS<CR>", { desc = "commit", exit = true, nowait = true } },
      { "<Enter>", "<Cmd>vertical Git<CR>", { nowait = true, exit = true, desc = "GitStatus" } },
      { "q", nil, { exit = true, nowait = true, desc = "exit" } },
    },
  })
end

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
    local function map(mode, l, r, opts)
      opts = opts or {}
      opts.buffer = bufnr
      vim.keymap.set(mode, l, r, opts)
    end

    setup_hydra(bufnr)

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
