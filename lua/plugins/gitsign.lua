require('gitsigns').setup {
    signs = {
        add = {
            hl = 'GitSignsAdd',
            text = '▎',
            numhl = 'GitSignsAddNr',
            linehl = 'GitSignsAddLn'
        },
        change = {
            hl = 'GitSignsChange',
            text = '░',
            numhl = 'GitSignsChangeNr',
            linehl = 'GitSignsChangeLn'
        },
        delete = {
            hl = 'GitSignsDelete',
            text = '_',
            numhl = 'GitSignsDeleteNr',
            linehl = 'GitSignsDeleteLn'
        },
        topdelete = {
            hl = 'GitSignsDelete',
            text = '‾',
            numhl = 'GitSignsDeleteNr',
            linehl = 'GitSignsDeleteLn'
        },
        changedelete = {
            hl = 'GitSignsChange',
            text = '▒',
            numhl = 'GitSignsChangeNr',
            linehl = 'GitSignsChangeLn'
        }
    },
    keymaps = {
        -- Default keymap options
        noremap = true,
        ['n gin'] = {
            expr = true,
            "&diff ? ']c' : '<cmd>lua require\"gitsigns.actions\".next_hunk()<CR>'"
        },
        ['n gim'] = {
            expr = true,
            "&diff ? '[c' : '<cmd>lua require\"gitsigns.actions\".prev_hunk()<CR>'"
        },

        ['n gis'] = '<cmd>lua require"gitsigns".stage_hunk()<CR>',
        ['n giu'] = '<cmd>lua require"gitsigns".undo_stage_hunk()<CR>',
        ['n gir'] = '<cmd>lua require"gitsigns".reset_hunk()<CR>',
        ['n gip'] = '<cmd>lua require"gitsigns".preview_hunk()<CR>',
        ['n gib'] = '<cmd>lua require"gitsigns".blame_line(true)<CR>'
    },
    numhl = true,
    linehl = false,
    watch_gitdir = {interval = 1000, follow_files = true},
    current_line_blame = false,
    current_line_blame_opts = {
        virt_text = true,
        virt_text_pos = 'eol', -- 'eol' | 'overlay' | 'right_align'
        delay = 1000
    },
    sign_priority = 6,
    update_debounce = 100,
    status_formatter = nil, -- Use default
    word_diff = false,
    diff_opts = {internal = true}
}
