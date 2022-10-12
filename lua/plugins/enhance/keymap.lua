local map = require("editor.utils").map
local nmap = require("editor.utils").nmap

--
-- EasyAlign
--
map("v", "<space>e", ":EasyAlign<CR>")

--
-- nvim-tree
--
nmap("<leader>t", ":NvimTreeToggle<CR>")
nmap("<space>t", function()
  local view = require("nvim-tree.view")
  if view.is_visible() then
    view.close()
  else
    require("nvim-tree").open_replacing_current_buffer()
  end
end)

--
-- fterm
--
-- float terminal
nmap("<C-\\>", [[:ToggleTerm direction=float<CR>:startinsert<CR>]])
map("t", "<C-\\>", [[<C-\><C-n>:ToggleTerm<CR>]])
-- horizontal terminal
nmap("_", [[:ToggleTerm direction=horizontal<CR>:startinsert<CR>]])
map("t", "<C-n>", [[<C-\><C-n>]])
-- terminal windows movement
map("t", "<C-k>", [[<C-\><C-n><C-w>k]])

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

          local response = {
            ["live grep"] = "live_grep",
            ["buffer symbols"] = "lsp_document_symbols",
            ["workspace symbols"] = "lsp_workspace_symbols",
          }

          local func = response[selection[1]]
          if func == nil then
            return
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
nmap("<M-right>", [[<Cmd>BufferLineCycleNext<CR>]])

nmap("<M-left>", [[<Cmd>BufferLineCyclePrev<CR>]])
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
