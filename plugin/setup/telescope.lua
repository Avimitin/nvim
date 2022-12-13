if require("libs.g").telescope then
  return
end

local nmap = require("libs.keymaps").nmap
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
