--! This file must contains non-blocking setup functions

local setups_sets = {
  ["dap"] = function()
    require("overlays.rc.dapcmds")
  end,
  ["multi_cursor"] = function()
    -- visual multi mappings
    -- clean the keymap `u` and initialize the new keymap set
    require("libs.keymaps").map("", "u", "<nop>")

    -- u is map to <C-z>, let us reuse it here
    vim.g.VM_maps = {
      ["Find Under"] = "un",
      ["Find Subword Under"] = "un",
      ["Select Cursor Down"] = "<C-down>",
      ["Select Cursor Up"] = "<C-up>",
      ["Select All"] = "uA",
      ["Undo"] = "<C-z>",
      ["Redo"] = "<C-r>",
      ["Start Regex Search"] = "ux",
      ["Visual Regex"] = "ux",
      ["Visual All"] = "uA",
      ["Visual Add"] = "ua",
      ["Visual Find"] = "uf",
      ["Visual Cursors"] = "uc",
    }
  end,
  ["wildfire"] = function()
    vim.g.wildfire_objects = { "i'", 'i"', "i)", "i]", "i}", "ip", "it", "i`" }
  end,
  ["dashboard"] = function()
    vim.api.nvim_create_autocmd("Vimenter", {
      group = vim.api.nvim_create_augroup("dashboard_cond_load", { clear = true }),
      nested = true,
      callback = function()
        if vim.fn.argc() == 0 and vim.fn.line2byte("$") == -1 then
          require("packer").loader("dashboard-nvim")
          require("overlays.rc.dashboard")
        end
      end,
    })
  end,
  ["matchup"] = function()
    vim.g.matchup_matchparen_offscreen = {}
    require("libs.keymaps").map({ "n", "x", "o" }, ",", "<Plug>(matchup-%)")
  end,
  ["crates"] = function()
    vim.api.nvim_create_autocmd("BufRead", {
      pattern = "Cargo.toml",
      callback = function(props)
        require("packer").loader("crates.nvim")
        require("overlays.rc.crates").setup_hydra(props.buf)
      end,
    })
  end,
  ["telescope"] = function()
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
  end, -- END of telescope
  ["bufferline"] = function()
    local nmap = require("libs.keymaps").nmap
    nmap("<C-c>", ":BufferLinePickClose<CR>") -- close tab
    -- move between tabs
    nmap("<Tab>", "<CMD>BufferLineCycleNext<CR>")
    nmap("<S-Tab>", [[<Cmd>BufferLineCyclePrev<CR>]])

    nmap("<leader>p", [[<CMD>:BufferLinePick<CR>]])
    -- move tabs
    nmap("<M-S-right>", [[<CMD>BufferLineMoveNext<CR>]])
    nmap("<M-S-left>", [[<CMD>BufferLineMovePrev<CR>]])
  end,
  ["toggleterm"] = function()
    local map = require("libs.keymaps").map
    local nmap = require("libs.keymaps").nmap
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
  end,
  ["easyalign"] = function()
    require("libs.keymaps").map("v", "<space>e", ":EasyAlign<CR>")
  end,
  ["dial"] = function()
    local nmap = require("libs.keymaps").nmap
    local map = require("libs.keymaps").map
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
  end,
  ["nvim_tree"] = function()
    require("libs.keymaps").nmap("<leader>t", ":NvimTreeToggle<CR>")
  end,
  ["gitsigns"] = function()
    vim.api.nvim_create_autocmd({ "BufAdd", "VimEnter" }, {
      callback = function()
        local function onexit(code, _)
          if code == 0 then
            vim.schedule(function()
              require("packer").loader("gitsigns.nvim")
              require("overlays.rc.gitsigns")
              require("scrollbar.handlers.gitsigns").setup()
            end)
          end
        end

        vim.loop.spawn("git", {
          args = {
            "ls-files",
            "--error-unmatch",
            vim.fn.expand("%"),
          },
        }, onexit)
      end,
    })
  end,
  ["js_deps"] = function()
    vim.api.nvim_create_autocmd("BufRead", {
      pattern = "package.json",
      callback = function(props)
        require("packer").loader("package-info.nvim")
        require("overlays.rc.js_deps").setup_hydra(props.buf)
      end,
    })
  end,
  ["cinnamon"] = function()
    local map = require("libs.keymaps").map
    local function scroll(a1, a2, a3, a4)
      return function()
        require("cinnamon.scroll").scroll(a1, a2, a3, a4)
      end
    end

    map({ "n", "x" }, "J", scroll("5j", 1, 1))
    map({ "n", "x" }, "K", scroll("5k", 1, 1))
    map({ "n", "x" }, "gg", scroll("gg", 1))
    map({ "n", "x" }, "G", scroll("G", 1, 1))
  end,
}

return setups_sets
