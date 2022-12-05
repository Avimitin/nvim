-- map create a new mapping
---@param mode string|table specify vim mode
---@param lhs string specify the new keymap
---@param rhs string|function specify the keymap or commands
---@param opts table|nil setting options. Default: { noremap = true, silent = true, expr = false }
local function map(mode, lhs, rhs, opts)
  local options = {
    noremap = true,
    silent = true,
    expr = false,
  }
  if opts then
    options = vim.tbl_extend("force", options, opts)
  end
  if type(rhs) == "function" then
    options.callback = rhs
    rhs = ""
  end
  local stat, error = pcall(vim.keymap.set, mode, lhs, rhs, options)
  if not stat then
    vim.notify(error, vim.log.levels.ERROR, {
      title = "keymap",
    })
  end
end

local function nmap(...)
  map("n", ...)
end

local function xmap(...)
  map("x", ...)
end

local function imap(...)
  map("i", ...)
end

local d = function(s)
  return { desc = s }
end

vim.g.mapleader = ";"

-- quicker motion
nmap("J", "5j", d("Jump 5 lines down"))
xmap("J", "5j", d("Jump 5 lines down"))

nmap("K", "5k", d("Jump 5 lines up"))
xmap("K", "5k", d("Jump 5 lines up"))

-- Emacs key mapping in insert mode
imap("<C-a>", "<Home>")
imap("<C-e>", "<End>")
imap("<C-b>", "<ESC>bi")
imap("<C-f>", "<ESC>wa")
imap("<C-n>", "<ESC>ja")
imap("<C-p>", "<ESC>ka")

nmap("L", "g_", d("Jump to the end of the character"))
nmap("H", "^", d("Jump to the beginning of the character"))

xmap("L", "g_", d("Jump to the end of the character"))
xmap("H", "^", d("Jump to the beginning of the character"))

nmap("W", "5w", d("Jump 5 word forward"))
nmap("B", "5b", d("Jump 5 word backward"))

-- no more background key
nmap("<C-z>", "u", d("Revert change"))

-- move block easily
nmap("<", "<<", d("Decrease indent"))
nmap(">", ">>", d("Increase indent"))
xmap("<", "<gv", d("Increase indent"))
xmap(">", ">gv", d("Decrease indent"))

-- create tab like window
nmap("<C-T>h", ":tabprevious<CR>", d("Goto previous tab"))
nmap("<C-T>l", ":tabnext<CR>", d("Goto next tab"))
nmap("<C-T>n", ":tabnew<CR>", d("Create a new tab"))

-- save quickly
nmap(";w", ":w<CR>", d("Save buffer"))

-- shut down the search high light
nmap("<ESC>", ":nohlsearch<CR>", d("Close search highlight"))
-- no more finger expansion
map("i", "<A-;>", "<ESC>", d("Exit the insert mode"))

-- kill buffer with ;q , quit window with :q.
nmap(";q", require("plugins.libs.bufdel").delete_buffer)

-- Write and quit. Alias for :wq<CR>
nmap(";x", ":x<CR>")

-- % is so hard to reach...
map({ "n", "x", "o" }, ",", "%", { noremap = false, silent = false })

-- paste from system clipboard
nmap("<C-p>", [["+p]])

--
-- EasyAlign
--
map("v", "<space>e", ":EasyAlign<CR>")

--
-- nvim-tree
--
nmap("<leader>t", ":NvimTreeToggle<CR>")

--
-- fterm
--
-- float terminal
nmap("<C-\\>", [[:ToggleTerm direction=float<CR>]])
map("t", "<C-\\>", [[<C-\><C-n>:ToggleTerm<CR>]])
-- horizontal terminal
nmap("_", [[:ToggleTerm direction=horizontal<CR>]])
map("t", "<A-;>", [[<C-\><C-n>]])
-- terminal windows movement
map("t", "<C-k>", [[<C-\><C-n><C-w>k]])
map("t", "<C-l>", [[<C-\><C-n><C-w>l]])
map("t", "<C-h>", [[<C-\><C-n><C-w>h]])

--
-- telescope
--
nmap("<leader>f", function()
  require("telescope.builtin").find_files(require("telescope.themes").get_ivy())
end)

nmap("<leader>s", function()
  local pickers = require("telescope.pickers")
  local finders = require("telescope.finders")
  local conf = require("telescope.config").values
  local actions = require("telescope.actions")
  local action_state = require("telescope.actions.state")
  local builtin = require("telescope.builtin")
  local opts = {
    sorting_strategy = "ascending",
    results_title = false,
    layout_strategy = "center",
    layout_config = {
      width = 50,
      height = 9,
    },
    borderchars = {
      prompt = { "─", "│", " ", "│", "╭", "╮", "│", "│" },
      results = { "─", "│", "─", "│", "├", "┤", "╯", "╰" },
      preview = { "─", "│", "─", "│", "╭", "╮", "╯", "╰" },
    },
  }

  pickers
    .new(opts, {
      prompt_title = "Search",
      finder = finders.new_table({
        results = { "live grep", "buffer symbols", "workspace symbols" },
      }),
      sorter = conf.generic_sorter(opts),
      attach_mappings = function(prompt_bufnr, _)
        actions.select_default:replace(function()
          actions.close(prompt_bufnr)
          local selection = action_state.get_selected_entry()
          if not selection then
            vim.notify("Illegal Selection!", vim.log.levels.ERROR)
            return false
          end

          local response = {
            ["live grep"] = "live_grep",
            ["buffer symbols"] = "lsp_document_symbols",
            ["workspace symbols"] = "lsp_workspace_symbols",
          }

          local func = response[selection[1]]
          if func == nil then
            return false
          end

          builtin[func](require("telescope.themes").get_ivy())
        end)
        return true
      end,
    })
    :find()
end)

--
-- bufferline
--
nmap("<C-c>", ":BufferLinePickClose<CR>") -- close tab
-- move between tabs
nmap("<Tab>", "<CMD>BufferLineCycleNext<CR>")
nmap("<S-Tab>", [[<Cmd>BufferLineCyclePrev<CR>]])

nmap("<leader>p", [[<CMD>:BufferLinePick<CR>]])
-- move tabs
nmap("<M-S-right>", [[<CMD>BufferLineMoveNext<CR>]])
nmap("<M-S-left>", [[<CMD>BufferLineMovePrev<CR>]])

--
-- dispatch
--
nmap("<leader>d", ":Dispatch ", { noremap = true, silent = false })

--
-- vim-matchup
--
map({ "n", "x", "o" }, ",", "<Plug>(matchup-%)")

--
-- dial.nvim
--
local function _cmd_sequence(direction, mode)
  local function cmd(body)
    local cmd_sequences = "<Cmd>"
    local cr_sequences = "<CR>"
    return cmd_sequences .. body .. cr_sequences
  end

  local function if_expr(cond, branch_true, branch_false)
    if cond then
      return branch_true
    end
    return branch_false
  end

  local select = cmd([[lua require"dial.command".select_augend_]] .. mode .. "()")
  local setopfunc = cmd([[let &opfunc="dial#operator#]] .. direction .. "_" .. mode .. [["]])
  local textobj = if_expr(mode == "normal", cmd([[lua require("dial.command").textobj()]]), "")
  return select .. setopfunc .. "g@" .. textobj
end

nmap("=", _cmd_sequence("increment", "normal"))
nmap("-", _cmd_sequence("decrement", "normal"))
map("v", "=", _cmd_sequence("increment", "visual"))
map("v", "-", _cmd_sequence("decrement", "visual"))

return {
  map = map,
  nmap = nmap,
  imap = imap,
  xmap = xmap,
}
