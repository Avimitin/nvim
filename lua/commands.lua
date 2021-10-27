local function new_cmd(cmd, repl)
  vim.cmd("command "..cmd.." "..repl)
end

new_cmd("ClipRec", [[lua require('neoclip').start()]])
new_cmd("ClipView", [[Telescope neoclip]])

new_cmd("ViewSession", [[Telescope sessions]])
