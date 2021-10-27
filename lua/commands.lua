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
