local new_cmd = require('utils').new_cmd
-- plugin neoclip
new_cmd("ClipRec", [[lua require('neoclip').start()]])
new_cmd("ClipView", [[Telescope neoclip]])

-- plugin lua formmater
new_cmd("LuaFormat", [[call LuaFormat()]])

-- plugin focus
new_cmd("FSplit", [[FocusSplitNicely]])

new_cmd("BufCL", [[BufferLineCloseLeft]])
new_cmd("BufCR", [[BufferLineCloseRight]])

new_cmd("DapUIToggle", [[lua require("dapui").toggle()]])

new_cmd("DapBreakpoint", [[lua require("dap").toggle_breakpoint()]])
new_cmd("DapBp", [[lua require("dap").toggle_breakpoint()]])

new_cmd("DapContinue", [[lua require("dap").continue()]])
new_cmd("DapC", [[lua require("dap").continue()]])

new_cmd("DapStepOver", [[lua require("dap").step_over()]])

new_cmd("Glog", [[Git log --oneline]])
