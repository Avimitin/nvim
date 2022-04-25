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
alias("DapUIToggle", function()
  require("dapui").toggle()
end)

-- debug function
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

-- Crate.nvim
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

-- nvim-spectre
alias("SpectreOpen", function()
  require("spectre").open()
end)

alias("HiCurLine", [[call matchadd('HighlightLineMatches', '\%'.line('.').'l')]])
alias("HiCurLineOff", [[call clearmatches()]])

alias("Glog", function()
  -- It will check if the vim-fugitive is loaded, so don't worry
  require("packer").loader("vim-fugitive")
  require("packer").loader("vim-flog")
  vim.cmd("Flog")
end)

alias("GlogS", function()
  -- It will check if the vim-fugitive is loaded, so don't worry
  require("packer").loader("vim-fugitive")
  require("packer").loader("vim-flog")
  vim.cmd("Flogsplit")
end)
