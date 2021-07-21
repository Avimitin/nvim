function Map(mode, lhs, rhs, opts)
    local options = {noremap = true, silent = true}
    if opts then
        options = vim.tbl_extend("force", options, opts)
    end
    local stat = pcall(vim.api.nvim_set_keymap, mode, lhs, rhs, options)
		if not stat then
			print(lhs)
			print(rhs)
			print("illegal")
		end
end

Map("",  "K",     "5k")
Map("",  "J",     "5j")
Map("",  "L",     "$")
Map("",  "H",     "0")
Map("",  "X",     "Vx")
Map("",  "W",     "5w")
Map("",  "B",     "5b")
Map("",  "vw",    "viw")
Map("",  "<C-z>", "u")
Map("n", "<",     "<<")
Map("n", ">",     ">>")
Map("",  "s",     "<nop>")
Map("",  "-",     "N")
Map("",  "=",     "n")

vim.g.mapleader=" "

Map("n", "<LEADER>s", ":w<CR>")
Map("n", "<C-s>",     ":w<CR>")

Map("n", "<LEADER>q", ":wq<CR>")

Map("n", "<C-q>",     ":q<CR>")

Map("v", "<LEADER>y", [["+y]])

Map("",  "<LEADER>p", [["+p]])

Map("n", "<ESC>",     ":nohlsearch<CR>")

Map("i", "jk",        "<ESC>")
Map("i", "jj",        "<ESC>")
Map("v", "jk",        "<ESC>")

Map("n", "spv",       "<C-w>t<C-w>H")

Map("n", "srr",       "<C-w>b<C-w>K")
Map("n", "srv",       "<C-w>b<C-w>H")

Map("n", "<up>",      ":res +5<CR>")
Map("n", "<down>",    ":res -5<CR>")
Map("n", "<left>",    ":vertical resize-5<CR>")
Map("n", "<right>",   ":vertical resize+5<CR>")

Map("n", "sk",        "<C-w>k")
Map("n", "sj",        "<C-w>j")
Map("n", "sh",        "<C-w>h")
Map("n", "sl",        "<C-w>l")

Map("i", "<C-c>",     "<ESC>zzi")

function SetCompleteKey()
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
  Map('i', '<Tab>', 'v:lua.tab_complete()', { expr = true })
  Map('s', '<Tab>', 'v:lua.tab_complete()', { expr = true })
  Map('i', '<S-Tab>', 'v:lua.s_tab_complete()', { expr = true })
  Map('s', '<S-Tab>', 'v:lua.s_tab_complete()', { expr = true })

  -- Map compe confirm and complete functions
  Map('i', '<cr>', 'compe#confirm("<cr>")', { expr = true })
  Map('i', '<c-space>', 'compe#complete()', { expr = true })
end

--[[==============================================
-- gitgutter settings
--===============================================]]
Map('n', 'gir', '<cmd>lua require"gitsigns".reset_hunk()<CR>')
Map('n', 'giu', '<cmd>lua require"gitsigns".undo_stage_hunk()<CR>')
Map('n', 'gis', '<cmd>lua require"gitsigns".stage_hunk()<CR>')
Map('n', 'gip', '<cmd>lua require"gitsigns".preview_hunk()<CR>')
Map('n', 'gib', '<cmd>lua require"gitsigns".blame_line(true)<CR>')
Map('n', 'gin', [[&diff ? ']c' : '<cmd>lua require\"gitsigns.actions\".next_hunk()<CR>']], { expr = true })
Map('n', 'gim', [[&diff ? '[c' : '<cmd>lua require\"gitsigns.actions\".prev_hunk()<CR>']], { expr = true })

--[[==============================================
-- vim-go settings
--===============================================]]
vim.api.nvim_command ( [[
	autocmd BufWrite *.go GoImports
	autocmd FileType go nmap <silent> got :GoTestFunc<CR>
	autocmd FileType go nmap <silent> gor :GoRun<CR>
]] )

--lazygit
--use fterm to open the lazygit
local term = require("FTerm.terminal")
local lazygit = term:new():setup({
    cmd = "lazygit",
    dimensions = {
        height = 0.9,
        width = 0.9
    }
})
function _G.fterm_lazygit()
    lazygit:toggle()
end
Map('n', '<C-g>', ':lua _G.fterm_lazygit()<CR>')

--anyjump
Map('n', '<leader>aj', ':AnyJump<CR>')
Map('n', '<leader>ab', ':AnyJumpBack<CR>')

--easymotion
Map('n', 'u'        , ':HopChar2<CR>')
Map('n', '<Leader>j', ':HopLine<CR>')
Map('n', '<Leader>k', ':HopLine<CR>')

Map('n', '<Leader>o', ':NnnPicker %:p:h<CR>')
