require('nvim-autopairs').setup({enable_check_bracket_line = false})
require('nvim-autopairs.completion.cmp').setup {
      map_complete = true,
      map_cr = true,
}

local npairs = require("nvim-autopairs")
local Rule = require('nvim-autopairs.rule')

npairs.setup({
    check_ts = true,
    ts_config = {
        lua = {'string'}, -- it will not add pair on that treesitter node
        javascript = {'template_string'},
        java = false -- don't check treesitter on java
    },
    fast_wrap = {
        map = '\\e',
        chars = {'{', '[', '(', '"', "'"},
        pattern = string.gsub([[ [%'%"%)%>%]%)%}%,] ]], '%s+', ''),
        end_key = '$',
        keys = 'qwertyuiopzxcvbnmasdfghjkl',
        check_comma = true,
        hightlight = 'Search'
    }
})

require('nvim-treesitter.configs').setup {autopairs = {enable = true}}

local ts_conds = require('nvim-autopairs.ts-conds')

-- press % => %% is only inside comment or string
npairs.add_rules({
    Rule("%", "%", "lua"):with_pair(ts_conds.is_ts_node({'string', 'comment'})),
    Rule("$", "$", "lua"):with_pair(ts_conds.is_not_ts_node({'function'})),
    Rule(' ', ' '):with_pair(function(opts)
        local pair = opts.line:sub(opts.col - 1, opts.col)
        return vim.tbl_contains({'()', '[]', '{}'}, pair)
    end), Rule('( ', ' )'):with_pair(function() return false end):with_move(
        function(opts) return opts.prev_char:match('.%)') ~= nil end):use_key(
        ')'),
    Rule('{ ', ' }'):with_pair(function() return false end):with_move(
        function(opts) return opts.prev_char:match('.%}') ~= nil end):use_key(
        '}'), Rule('[ ', ' ]'):with_pair(function() return false end):with_move(
        function(opts) return opts.prev_char:match('.%]') ~= nil end):use_key(
        ']')
})
