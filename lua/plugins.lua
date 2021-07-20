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
  map("n", "<S-x>", ":bd!<CR>", opt) -- close tab

  -- move between tabs
  map("n", ".", [[<Cmd>BufferLineCycleNext<CR>]], opt)
  map("n", ",", [[<Cmd>BufferLineCyclePrev<CR>]], opt)
end

local tree_stat = pcall(require, "plugins.nvimtree")
if tree_stat then
  map("n", "tt", ":NvimTreeToggle<CR>", {})
end

pcall(require, "plugins.lsp")

local compe_stat = pcall(require, "plugins.compe")
if compe_stat then
  -- Map tab to the above tab complete functions
  map('i', '<Tab>', 'v:lua.tab_complete()', { expr = true })
  map('s', '<Tab>', 'v:lua.tab_complete()', { expr = true })
  map('i', '<S-Tab>', 'v:lua.s_tab_complete()', { expr = true })
  map('s', '<S-Tab>', 'v:lua.s_tab_complete()', { expr = true })

  -- Map compe confirm and complete functions
  map('i', '<cr>', 'compe#confirm("<cr>")', { expr = true })
  map('i', '<c-space>', 'compe#complete()', { expr = true })
end
