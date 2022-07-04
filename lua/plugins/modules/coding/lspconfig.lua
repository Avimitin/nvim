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

local on_attach = function(client, bufnr)
  local function bmap(...)
    vim.api.nvim_buf_set_keymap(bufnr, ...)
  end

  local function buf_set_option(...)
    vim.api.nvim_buf_set_option(bufnr, ...)
  end

  -- Enable completion triggered by <c-x><c-o>
  buf_set_option("omnifunc", "v:lua.vim.lsp.omnifunc")

  -- Mappings.
  local opts = {
    noremap = true,
    silent = true,
  }
  bmap("n", "gd", "<Cmd>Lspsaga lsp_finder<CR>", opts)
  bmap("n", "gp", "<Cmd>Lspsaga preview_definition<CR>", opts)
  bmap("n", "gh", "<Cmd>Lspsaga hover_doc<CR>", opts)
  bmap("n", "<C-u>", "<cmd>lua require('lspsaga.action').smart_scroll_with_saga(-1)<cr>", opts)
  bmap("n", "<C-d>", "<cmd>lua require('lspsaga.action').smart_scroll_with_saga(1)<cr>", opts)
  bmap("n", "gs", "<cmd>Lspsaga signature_help<CR>", opts)
  bmap("n", "go", "<cmd>Lspsaga show_line_diagnostics<CR>", opts)
  bmap("n", "gj", "<cmd>Lspsaga diagnostic_jump_next<CR>", opts)
  bmap("n", "gk", "<cmd>Lspsaga diagnostic_jump_prev<CR>", opts)
  bmap("n", "gr", "<cmd>Lspsaga rename<CR>", opts)
  bmap("n", "ga", "<cmd>Lspsaga code_action<CR>", opts)

  -- most of the lsp server don't implement textDocument/Declaration, so gD is useless for now.
  bmap("n", "gD", "<Cmd>lua vim.lsp.buf.declaration()<CR>", opts)
  bmap("n", "gm", "<cmd>lua vim.lsp.buf.implementation()<CR>", opts)
  bmap("n", "gt", "<cmd>lua vim.lsp.buf.type_definition()<CR>", opts)
  bmap("n", "gq", "<cmd>lua vim.diagnostic.set_loclist()<CR>", opts)

  -- add rust specific keymappings
  if client.name == "rust_analyzer" then
    bmap("n", "<leader>rr", "<cmd>RustRunnables<CR>", opts)
    bmap("n", "<leader>ra", "<cmd>RustHoverAction<CR>", opts)
  end

  -- Set some keybinds conditional on server capabilities
  -- 0.8.0
  if vim.fn.has("nvim-0.8.0") then
    if client.server_capabilities.documentFormattingProvider then
      bmap("n", "gf", "<cmd>lua vim.lsp.buf.format({ async = true })<CR>", opts)
    elseif client.server_capabilities.documentRangeFormattingProvider then
      bmap("x", "gf", "<cmd>lua vim.lsp.buf.range_formatting()<CR>", opts)
    end

    -- 0.6.0 - 0.7.0
  else
    if client.resolved_capabilities.document_formatting then
      bmap("n", "gf", "<cmd>lua vim.lsp.buf.formatting()<CR>", opts)
    elseif client.resolved_capabilities.document_range_formatting then
      bmap("x", "gf", "<cmd>lua vim.lsp.buf.range_formatting()<CR>", opts)
    end
  end
end

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
    on_attach = on_attach,
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

vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(
  vim.lsp.diagnostic.on_publish_diagnostics,
  lsp_publish_diagnostics_options
)

vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, {
  border = "single",
})

vim.lsp.handlers["textDocument/signatureHelp"] = vim.lsp.with(vim.lsp.handlers.signature_help, {
  border = "single",
})

return {
  set_lsp_key = on_attach,
}
