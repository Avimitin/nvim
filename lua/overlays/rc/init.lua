local rc = {}

if require("editor.utils").vhas("nvim-0.8.0") then
  vim.g.matchup_matchparen_offscreen = {}
end

-- #######################################
-- #           Callback                  #
-- #######################################

rc.null_ls = function()
  require("overlays.rc.null_ls")
end

rc.treesitter = function()
  require("overlays.rc.treesitter")
end

rc.lspconfig = function()
  require("overlays.rc.lspconfig")
end

rc.lspsaga = function()
  require("overlays.rc.lspsaga")
end

rc.rust = function()
  require("overlays.rc.rust")
end

rc.dap = function()
  require("overlays.rc.dap")
end

rc.symbols_outline = function()
  require("overlays.rc.symbols_outline")
end

return rc
