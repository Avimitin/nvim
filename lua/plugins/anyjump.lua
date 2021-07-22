vim.g.any_jump_window_width_ratio  = 0.8
vim.g.any_jump_window_height_ratio = 0.9
vim.g.any_jump_disable_default_keybindings = 1

--anyjump
vim.api.nvim_set_keymap('n', '<leader>aj', ':AnyJump<CR>', {noremap=true, silent=true})
vim.api.nvim_set_keymap('n', '<leader>ab', ':AnyJumpBack<CR>',{noremap=true, silent=true})
