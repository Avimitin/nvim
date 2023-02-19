local register = require("pack").register

register("neovim/nvim-lspconfig", {
  lazy = true,
})

register("glepnir/lspsaga.nvim", {
  event = "LspAttach",
  cmd = "LspSaga",
  config = function()
    require("lsp.lspsaga")
  end,
})

local export = {}

function export.start(server, extra)
  local basic_config = require("lsp.config")
  if extra then
    basic_config.on_attach = extra.on_attach or require("lsp.keymaps")
    basic_config.settings = extra.settings or {}
  end

  local lspconfig = require("lspconfig")

  lspconfig[server].setup(basic_config)
  -- manually setup because FileType event is behind BufReadPost event
  lspconfig[server].manager.try_add_wrapper()
end

return export
