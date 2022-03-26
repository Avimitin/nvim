-- alias can help create new vim command.
-- @param cmd The user command
-- @param repl The actual command or function
-- @param force force create command? boolean
local alias = function(cmd, repl, force)
  local command
  if force then
    command = "command! " .. cmd .. " " .. repl
  else
    command = "command " .. cmd .. " " .. repl
  end
  local ok, err = pcall(vim.cmd, command)
  if not ok then
    vim.notify("setting cmd: " .. cmd .. " " .. err, vim.log.levels.ERROR, {
      title = "command",
    })
  end
end

-- plugin neoclip
alias("ClipRec", [[lua require('neoclip').start()]])
alias("ClipView", [[Telescope neoclip]])

-- run stylua in background
alias("LuaFormat", [[Dispatch! stylua %]])

-- close buffer
alias("BufCL", [[BufferLineCloseLeft]])
alias("BufCR", [[BufferLineCloseRight]])

-- debug ui
alias("DapUIToggle", [[lua require("dapui").toggle()]])

-- debug function
alias("DapBreakpoint", [[lua require("dap").toggle_breakpoint()]])
alias("DapBp", [[lua require("dap").toggle_breakpoint()]])

alias("DapContinue", [[lua require("dap").continue()]])
alias("DapC", [[lua require("dap").continue()]])

alias("DapStepOver", [[lua require("dap").step_over()]])

-- Crate.nvim
alias("CrateUpdate", [[lua require("crates").update_crate()]])
alias("CrateUpgrade", [[lua require("crates").upgrade_crate()]])
alias("CrateMenu", [[lua require("crates").show_popup()]])
alias("CrateReload", [[lua require("crates").reload()]])

-- nvim-spectre
alias("SpectreOpen", "lua require('spectre').open()")

alias("HiCurLine", [[call matchadd('HighlightLineMatches', '\%'.line('.').'l')]])
alias("HiCurLineOff", [[call clearmatches()]])
