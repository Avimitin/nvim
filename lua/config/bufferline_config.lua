require('bufferline').setup {
    options = {
        offsets = {{filetype = "NvimTree", text = " Explorer", padding = 1}},
        buffer_close_icon = "",
        modified_icon = "",
        close_icon = "",
        left_trunc_marker = "",
        right_trunc_marker = "",
        max_name_length = 14,
        max_prefix_length = 13,
        tab_size = 20,
        diagnostic = false,
        show_tab_indicators = true,
        enforce_regular_tabs = false,
        view = "multiwindow",
        show_buffer_close_icons = true,
        separator_style = "thin",
        always_show_bufferline = true
    }
}

local opt = {noremap = true, silent = true}
-- bufferline tab stuff
vim.api.nvim_set_keymap("n", "<A-t>", ":tabnew<CR>", opt) -- new tab
vim.api.nvim_set_keymap("n", "<C-c>", ":BufferLinePickClose<CR>", opt) -- close tab
vim.api.nvim_set_keymap("n", "<A-q>", [[<CMD>:bd<CR>]], opt)

-- move between tabs
vim.api.nvim_set_keymap("n", ".", [[<Cmd>BufferLineCycleNext<CR>]], opt)
vim.api.nvim_set_keymap("n", ",", [[<Cmd>BufferLineCyclePrev<CR>]], opt)

-- move tabs
vim.api.nvim_set_keymap("n", "<A->>", [[<CMD>BufferLineMoveNext<CR>]], opt)
vim.api.nvim_set_keymap("n", "<A-<>", [[<CMD>BufferLineMovePrev<CR>]], opt)
vim.api.nvim_set_keymap("n", "<A-p>", [[<CMD>:BufferLinePick<CR>]], opt)
