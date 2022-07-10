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

--
-- Crate.nvim
--
alias("CrateUpdate", function()
  require("crates").update_crate()
end)
alias("CrateUpgrade", function()
  require("crates").upgrade_crate()
end)
alias("CrateMenu", function()
  require("crates").show_popup()
end)
alias("CrateReload", function()
  require("crates").reload()
end)
