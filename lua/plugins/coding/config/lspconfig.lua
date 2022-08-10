local present1, lspconfig = pcall(require, "lspconfig")
local have_custom, custom = pcall(require, "custom")
if not present1 then
  vim.notify("Fail to load LSP", vim.log.levels.ERROR, {
    title = "plugins",
  })
  return
end

-- Gets a new ClientCapabilities object describing the LSP client
-- capabilities.
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

-- Add neovim built-in Lua library into lsp runtime path.
local neovim_lua_runtime_settings = {
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

-- Setup border for the floating window
local handlers = {
  ["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, {
    border = "single",
  }),

  ["textDocument/signatureHelp"] = vim.lsp.with(vim.lsp.handlers.signature_help, {
    border = "single",
  }),
}

-- Setup diagnostic icons and signs
vim.diagnostic.config({
  virtual_text = {
    prefix = "﮿",
    spacing = 4,
    source = "always",
  },
  signs = true,
  underline = true,
  -- update diagnostic in insert mode will be annoying when the output is too verbose
  update_in_insert = false,
})

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

-- Preconfigured language server that will be **automatically** installed.
--
-- WARNING: rust-analyzer is set up by plugin "rust-tools.nvim", *DONT*
-- configured it manually here.
--
-- User might define same server for multiple filetype, so here we use Set data structure
-- for unique value.
--
-- always enable sumneko_lua for Lua developing
local server_set = {
  sumneko_lua = 0,
}

-- IF
--   * we have a custom.lua file
--   * custom.lua file return a table which contains `langs` field
--   * the `langs` field has a Lua table value
--   * and the size of the `langs` field is not zero
-- THEN
--   we install and configure those language server
if have_custom and custom.langs and type(custom.langs) == "table" and #custom.langs > 0 then
  -- Insert a value into table if it is not presented in that table
  -- @field server: string
  function server_set:push(server)
    if self[server] == nil then
      self[server] = 0
    end
  end

  for _, lang in ipairs(custom.langs) do
    -- { "language filetype", "language server" }
    if type(lang) == "table" and #lang > 1 then
      -- lua 5.1 doesn't have continue keyword, so we have to write nested if block
      server_set:push(lang[2])
    end
  end

  server_set.push = nil
end

-- Attach the above settings to all the lspservers. And tell the nvim-lsp-installer to
-- install those servers when necessary.
for lspserver, _ in pairs(server_set) do
  local opts = {
    on_attach = require("plugins.coding.keymap").lsp_keymap,
    capabilities = capabilities,
    root_dir = vim.loop.cwd,
    handlers = handlers,
  }

  if lspserver == "sumneko_lua" then
    opts.settings = neovim_lua_runtime_settings
  end

  lspconfig[lspserver].setup(opts)
  vim.cmd([[ do User LspAttachBuffers ]])
end
