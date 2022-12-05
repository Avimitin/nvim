local alias = require("editor.utils").alias

--
-- debug ui
--
alias("DapUIToggle", function()
  require("dapui").toggle()
end)

--
-- debug function
--
-- TODO: setup the Hydra mode
alias("DapBreakpoint", function()
  require("dap").toggle_breakpoint()
end)
alias("DapBp", function()
  require("dap").toggle_breakpoint()
end)

alias("DapContinue", function()
  require("dap").continue()
end)
alias("DapC", function()
  require("dap").continue()
end)

alias("DapStepOver", function()
  require("dap").step_over()
end)
