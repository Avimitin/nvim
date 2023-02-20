local register = require("pack").register

-- Interact with LSP server
register("neovim/nvim-lspconfig", {
  lazy = true,
  config = function()
    -- Setup diagnostic icons and signs
    vim.diagnostic.config({
      virtual_text = {
        prefix = "",
        spacing = 6,
        format = function(diagnostic)
          return string.format(
            "%s  %s",
            vim.cfg.icons[vim.diagnostic.severity[diagnostic.severity]],
            diagnostic.message
          )
        end,
      },
      signs = true,
      underline = true,
      -- update diagnostic in insert mode will be annoying when the output is too verbose
      update_in_insert = false,
    })

    local types = {
      "Error",
      "Warn",
      "Hint",
      "Info",
    }

    for _, diag_type in ipairs(types) do
      local hl = "DiagnosticSign" .. diag_type
      vim.fn.sign_define(hl, {
        text = "",
        texthl = hl,
        numhl = hl,
        linehl = hl,
      })
    end
  end,
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
