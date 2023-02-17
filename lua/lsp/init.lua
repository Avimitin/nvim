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
