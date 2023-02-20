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

-- Pretty diagnostic quick fix panel
register("folke/trouble.nvim", {
  cmd = "TroubleToggle",
  config = function()
    require("trouble").setup({})
  end,
})

-- Rust specific plugin
register("simrat39/rust-tools.nvim", {
  lazy = true,
})

-- Cargo.toml manager
register("saecki/crates.nvim", {
  event = "BufRead Cargo.toml",
})

register("numToStr/Comment.nvim", {
  config = function()
    require("Comment").setup({})
  end,
  keys = {
    "gcc",
    "gbc",
    { mode = "x", "gc" },
    { mode = "x", "gb" },
  },
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
