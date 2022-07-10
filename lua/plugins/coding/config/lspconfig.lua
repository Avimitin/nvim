local present1, lspconfig = pcall(require, "lspconfig")
local have_custom, custom = pcall(require, "custom")
if not present1 then
  vim.notify("Fail to load LSP", vim.log.levels.ERROR, {
    title = "plugins",
  })
  return
end

-- [[ =================================================================================
--  LSP Settings
-- =================================================================================]]
-- Gets a new ClientCapabilities object describing the LSP client
-- capabilities.
local function setup_capabilities()
  local capabilities = vim.lsp.protocol.make_client_capabilities()
  capabilities.textDocument.completion.completionItem = {
    documentationFormat = {
      "markdown",
      "plaintext",
    },
    snippetSupport = true,
    preselectSupport = true,
    insertReplaceSupport = true,
    labelDetailsSupport = true,
    deprecatedSupport = true,
    commitCharactersSupport = true,
    tagSupport = {
      valueSet = { 1 },
    },
    resolveSupport = {
      properties = {
        "documentation",
        "detail",
        "additionalTextEdits",
      },
    },
  }

  return capabilities
end

local function neovim_lua_setting()
  return {
    Lua = {
      runtime = {
        -- Tell the language server which version of Lua you're using (most likely LuaJIT in the case of Neovim)
        version = "LuaJIT",
      },
      diagnostics = {
        -- Get the language server to recognize the `vim` global
        globals = { "vim" },
      },
      workspace = {
        -- Make the server aware of Neovim runtime files
        library = vim.api.nvim_get_runtime_file("", true),
      },
      -- Do not send telemetry data containing a randomized but unique identifier
      telemetry = {
        enable = false,
      },
    },
  }
end

-- [[ =================================================================================
--  LSP SETUP MAIN LOGIC
-- =================================================================================]]

require("nvim-lsp-installer").setup({
  -- only ensure Lua language server is installed
  ensure_installed = { "sumneko_lua" },
  automatic_installation = false,
  ui = {
    icons = {
      server_installed = "✓",
      server_pending = "➜",
      server_uninstalled = "✗",
    },
  },
})

-- Preconfigured language server, still need to use command to installed
-- `:LspInstall`
-- rust-analyzer is set up by plugin "rust-tools.nvim", *DONT* configured it manually here
local servers = {
  "sumneko_lua",
}

if have_custom and custom.lspconfig and custom.lspconfig.servers then
  servers = vim.list_extend(servers, custom.lspconfig.servers)
end

for _, v in ipairs(servers) do
  local opts = {
    on_attach = require("plugins.coding.keymap").lsp_keymap,
    capabilities = setup_capabilities(),
    root_dir = vim.loop.cwd,
  }
  if v == "sumneko_lua" then
    opts.settings = neovim_lua_setting()
  end

  lspconfig[v].setup(opts)
  vim.cmd([[ do User LspAttachBuffers ]])
end

-- [[ =================================================================================
-- CHORES
-- =================================================================================]]

local signs = {
  Error = " ",
  Warn = " ",
  Hint = " ",
  Info = " ",
}

for type, icon in pairs(signs) do
  local hl = "DiagnosticSign" .. type
  vim.fn.sign_define(hl, {
    text = icon,
    texthl = hl,
    numhl = "",
  })
end

local lsp_publish_diagnostics_options = {
  virtual_text = {
    prefix = "﮿",
    spacing = 4,
  },
  signs = true,
  underline = true,
  update_in_insert = false, -- update diagnostics insert mode
}

vim.lsp.handlers["textDocument/publishDiagnostics"] =
  vim.lsp.with(vim.lsp.diagnostic.on_publish_diagnostics, lsp_publish_diagnostics_options)

vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, {
  border = "single",
})

vim.lsp.handlers["textDocument/signatureHelp"] = vim.lsp.with(vim.lsp.handlers.signature_help, {
  border = "single",
})
