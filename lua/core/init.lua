local M = {}

-- Helper function to copy path and line(s)
local function copy_filepath(absolute)
  return function()
    -- 1. Get the current mode and file path
    local mode = vim.api.nvim_get_mode().mode
    local path = absolute and vim.fn.expand("%:p") or vim.fn.expand("%")

    -- 2. Default to the current cursor line
    local start_line = vim.fn.line(".")
    local end_line = start_line

    -- 3. If in visual mode, calculate the range
    -- The mode string will contain 'v', 'V', or '<C-v>' (\22)
    if mode:match("[vV\x16]") then
      local v_line = vim.fn.line("v") -- The line where visual selection started
      start_line = math.min(v_line, vim.fn.line("."))
      end_line = math.max(v_line, vim.fn.line("."))

      -- Gracefully exit visual mode after copying
      local esc = vim.api.nvim_replace_termcodes("<Esc>", true, false, true)
      vim.api.nvim_feedkeys(esc, "n", true)
    end

    -- 4. Format the string (e.g., path:10 or path:10-20)
    local line_suffix = start_line == end_line and tostring(start_line)
      or (start_line .. "-" .. end_line)
    local result = path .. ":" .. line_suffix

    -- 5. Copy to clipboard and notify
    vim.fn.setreg("+", result)
    print("Copied: " .. result)
  end
end

function M.setup(config)
  -- Reject loading plugins when bigfile detect. Default on 1.5M size.
  require("libs.bigfile").setup()

  require("core.options")
  require("core.autocmd")

  require("pack").setup(config or {})

  require("keys").mk_keymap({
    normal = {
      { "j", "v:count == 0 ? 'gj' : 'j'", desc = "Go display lines downward", expr = true },
      { "k", "v:count == 0 ? 'gk' : 'k'", desc = "Go display lines upward", expr = true },
      { "L", "g_", desc = "Jump to beginning" },
      { "H", "^", desc = "Jump to end" },
      {
        "<leader>w",
        function()
          vim.cmd.write({ bang = true })
        end,
        desc = "Save buffer",
      },
      { "<ESC>", vim.cmd.noh, desc = "Close search highlight" },
      {
        "<leader>q",
        function()
          require("libs.bufdel").delete_buffer_expr("", false)
        end,
        desc = "Close current buffer",
      },
      { "<leader>x", vim.cmd.x, desc = "Save and quit" },
      { "<C-p>", [["+p]], desc = "paste" },
      { "<C-S-v>", [["+p]], desc = "paste" },
      {
        "<leader>y",
        copy_filepath(false),
        desc = "Copy relative path and line to clipboard",
      },
      {
        "<leader>Y",
        copy_filepath(true),
        desc = "Copy absolute path and line to clipboard",
      },
    },
    selection = {
      { "J", ":m '>+1<CR>gv=gv" },
      { "K", ":m '<-2<CR>gv=gv" },
      { "L", "g_", desc = "Select to beginning" },
      { "H", "^", desc = "Select to end" },
      { "<C-z>", "<nop>", desc = "anti-touch" },
      { "<tab>", ">gv", desc = "Increase indent" },
      { "<s-tab>", "<gv", desc = "Decrease indent" },
      {
        "<leader>y",
        copy_filepath(false),
        desc = "Copy relative path and line to clipboard",
      },
      {
        "<leader>Y",
        copy_filepath(true),
        desc = "Copy absolute path and line to clipboard",
      },
    },
    insertion = {
      { "<C-a>", "<ESC>^i", desc = "Jump to beginning of the line" },
      { "<C-e>", "<End>", desc = "Jump to end of the line" },
      { "<M-;>", "<ESC>", desc = "Exit insert mode" },
      { "<C-p>", "<Up>", desc = "Go up one line" },
      { "<C-f>", "<Right>", desc = "Go up one line" },
      { "<C-b>", "<Left>", desc = "Go up one line" },
      { "<C-n>", "<Down>", desc = "Go down one line" },
      { "<C-z>", "<nop>", desc = "anti-touch" },
    },
    terminal = {
      { "<S-Space>", "<Space>" },
    },
  })

  vim.o.background = "dark"
  vim.cmd.colorscheme("kanagawa")

  require("lang").setup_lsp()
end

return M
