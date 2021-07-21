local function map(mode, lhs, rhs, opts)
    local options = {noremap = true, silent = true}
    if opts then
        options = vim.tbl_extend("force", options, opts)
    end
    vim.api.nvim_set_keymap(mode, lhs, rhs, options)
end

pcall(require, "plugins.treesitter")

local fterm_status = pcall(require, "plugins.fterm")
if fterm_status then
  map("n", "<C-\\>", [[<CMD>lua require("FTerm").open()<CR>]], {})
end

local bfl_status = pcall(require, "plugins.bufferline")
if bfl_status then
  local opt = {}
  -- bufferline tab stuff
  map("n", "<S-t>", ":tabnew<CR>", opt) -- new tab
  map("n", "<S-x>", ":BufferLinePickClose<CR>", opt) -- close tab

  -- move between tabs
  map("n", ".", [[<Cmd>BufferLineCycleNext<CR>]], opt)
  map("n", ",", [[<Cmd>BufferLineCyclePrev<CR>]], opt)

	-- move tabs
	map("n", "<A->>", [[<CMD>BufferLineMoveNext<CR>]], opt)
	map("n", "<A-<>", [[<CMD>BufferLineMovePrev<CR>]], opt)
end

local tree_stat = pcall(require, "plugins.nvimtree")
if tree_stat then
  map("n", "tt", ":NvimTreeToggle<CR>", {})
end

pcall(require, "plugins.lsp")

local compe_stat = pcall(require, "plugins.compe")
if compe_stat then
  -- Utility functions for compe and luasnip
  local t = function(str)
    return vim.api.nvim_replace_termcodes(str, true, true, true)
  end

  local check_back_space = function()
    local col = vim.fn.col '.' - 1
    if col == 0 or vim.fn.getline('.'):sub(col, col):match '%s' then
      return true
    else
      return false
    end
  end

  -- Use (s-)tab to:
  --- move to prev/next item in completion menu
  --- jump to prev/next snippet's placeholder
  local snip_stat, luasnip = pcall(require, 'luasnip')

  _G.tab_complete = function()
    if vim.fn.pumvisible() == 1 then
      return t '<C-n>'
    elseif snip_stat and luasnip.expand_or_jumpable() then
      return t '<Plug>luasnip-expand-or-jump'
    elseif check_back_space() then
      return t '<Tab>'
    else
      return vim.fn['compe#complete']()
    end
  end

  _G.s_tab_complete = function()
    if vim.fn.pumvisible() == 1 then
      return t '<C-p>'
    elseif snip_stat and luasnip.jumpable(-1) then
      return t '<Plug>luasnip-jump-prev'
    else
      return t '<S-Tab>'
    end
  end

  -- Map tab to the above tab complete functions
  map('i', '<Tab>', 'v:lua.tab_complete()', { expr = true })
  map('s', '<Tab>', 'v:lua.tab_complete()', { expr = true })
  map('i', '<S-Tab>', 'v:lua.s_tab_complete()', { expr = true })
  map('s', '<S-Tab>', 'v:lua.s_tab_complete()', { expr = true })

  -- Map compe confirm and complete functions
  map('i', '<cr>', 'compe#confirm("<cr>")', { expr = true })
  map('i', '<c-space>', 'compe#complete()', { expr = true })
end

local ts_stat = pcall(require, "plugins.telescope")
if ts_stat then
  map('n', '<leader>ff', [[<cmd>Telescope find_files<cr>]], {})
  map('n', '<leader>fg', [[<cmd>Telescope live_grep<cr>]], {})
  map('n', '<leader>fb', [[<cmd>Telescope buffers<cr>]], {})
  map('n', '<leader>fh', [[<cmd>Telescope help_tags<cr>]], {})
end

require("plugins.indent")
