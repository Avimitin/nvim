local register = require("pack").register

-- Interact with LSP server
register("neovim/nvim-lspconfig", {
  lazy = true,
})

-- UI for builtin LSP function
register("glepnir/lspsaga.nvim", {
  event = "LspAttach",
  cmd = "LspSaga",
  config = function()
    require("lsp.lspsaga")
  end,
})

-- Inject more LSP sources
register("jose-elias-alvarez/null-ls.nvim", {
  lazy = true,
})

local export = {}

function export.start(server, extra)
  local config = require("lsp.config")
  if extra then
    config.on_attach = extra.on_attach or require("lsp.keymaps")
    config.settings = extra.settings or {}
  end

  local lspconfig = require("lspconfig")

  lspconfig[server].setup(config)
  -- manually setup because FileType event is behind BufReadPost event
  lspconfig[server].manager.try_add_wrapper()
end

return export
