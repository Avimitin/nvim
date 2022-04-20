local alias = function(new, old, opts)
  local option = {}
  if opts then
    option = opts
  end
  vim.api.nvim_create_user_command(new, old, option)
end

-- plugin neoclip
alias("ClipRec", function()
  require("neoclip").start()
  require("telescope").load_extension("neoclip")
end)

alias("ClipView", [[Telescope neoclip]])

-- run stylua in background
alias("LuaFormat", [[Dispatch! stylua %]])

-- close buffer
alias("BufCL", [[BufferLineCloseLeft]])
alias("BufCR", [[BufferLineCloseRight]])

-- debug ui
alias("DapUIToggle", require("dapui").toggle)

-- debug function
alias("DapBreakpoint", require("dap").toggle_breakpoint)
alias("DapBp", require("dap").toggle_breakpoint)

alias("DapContinue", require("dap").continue)
alias("DapC", require("dap").continue)

alias("DapStepOver", require("dap").step_over)

-- Crate.nvim
alias("CrateUpdate", require("crates").update_crate)
alias("CrateUpgrade", require("crates").upgrade_crate)
alias("CrateMenu", require("crates").show_popup)
alias("CrateReload", require("crates").reload)

-- nvim-spectre
alias("SpectreOpen", require("spectre").open)

alias("HiCurLine", [[call matchadd('HighlightLineMatches', '\%'.line('.').'l')]])
alias("HiCurLineOff", [[call clearmatches()]])

alias("Glog", function()
  -- It will check if the vim-fugitive is loaded, so don't worry
  require("packer").loader("vim-fugitive")
  vim.cmd("Flog")
end)
