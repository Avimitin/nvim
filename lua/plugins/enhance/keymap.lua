local map = require("editor.utils").map
local nmap = require("editor.utils").nmap

--
-- EasyAlign
--
map("v", "<leader>e", ":EasyAlign<CR>")

--
-- nvim-tree
--
nmap(";t", ":NvimTreeToggle<CR>")

--
-- fterm
--
-- float terminal
nmap("<C-\\>", [[:ToggleTerm direction=float<CR>:startinsert<CR>]])
map("t", "<C-\\>", [[<C-\><C-n>:ToggleTerm<CR>]])
-- horizontal terminal
nmap("<M-`>", [[:ToggleTerm direction=horizontal<CR>:startinsert<CR>]])
map("t", "<M-`>", [[<C-\><C-n>:ToggleTerm<CR>]])
map("t", "<C-n>", [[<C-\><C-n>]])
-- terminal windows movement
map("t", "<C-k>", [[<C-\><C-n><C-w>k]])

--
-- telescope
--
nmap(";f", function()
  require("telescope.builtin").find_files(require("telescope.themes").get_ivy())
end)

nmap(";s", function()
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

nmap(";p", [[<CMD>:BufferLinePick<CR>]])
-- move tabs
nmap("<M-S-right>", [[<CMD>BufferLineMoveNext<CR>]])
nmap("<M-S-left>", [[<CMD>BufferLineMovePrev<CR>]])

--
-- dispatch
--
nmap(";d", ":Dispatch ", { noremap = true, silent = false })

--
-- vim-matchup
--
map({ "n", "x", "o" }, ",", "<Plug>(matchup-%)")
