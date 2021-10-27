local function new_cmd(cmd, repl)
  vim.cmd("command "..cmd.." "..repl)
end

-- plugin neoclip
new_cmd("ClipRec", [[lua require('neoclip').start()]])
new_cmd("ClipView", [[Telescope neoclip]])

-- plugin neovim-session-manager
new_cmd("ViewSession", [[Telescope sessions]])

-- plugin lua formmater
new_cmd("LuaFormat", [[call LuaFormat()]])

-- plugin focus
new_cmd("FSplit", [[FocusSplitNicely]])

-- telescope
new_cmd('Tsf', [[lua require('telescope.builtin').find_files()]])
new_cmd('Tsg', [[lua require('telescope.builtin').live_grep()]])
new_cmd('Tsb', [[lua require('telescope.builtin').buffers()]])
new_cmd('Tst', [[lua require('telescope.builtin').help_tags()]])

-- lazygit
new_cmd('Lg', [[lua _G.fterm_lazygit()]])
new_cmd('Ng', [[Neogit]])

