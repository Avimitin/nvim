local alias = function(cmd, exec, opt)
  local o = opt or {}
  vim.api.nvim_create_user_command(cmd, exec, o)
end

--
-- debug ui
--
alias("DapUIToggle", function()
  local Hydra = require("hydra")
  local dap = require("dap")
  require("dapui").toggle()

  local hint = [[
 _B_: Breakpoint 
 _C_: Continue
 _S_: Step over
 _q_: exit
]]

  Hydra({
    name = "Dap Debugger",
    hint = hint,
    config = {
      color = "pink",
      invoke_on_body = true,
      hint = {
        border = "rounded",
        position = "middle-right",
      },
    },
    mode = { "n" },
    body = "<leader>dp",
    heads = {
      { "B", dap.toggle_breakpoint, { silent = true } },
      { "C", dap.continue, { silent = true } },
      { "S", dap.step_over, { silent = true } },
      { "q", nil, { exit = true, nowait = true, desc = "exit" } },
    },
  })
end)
