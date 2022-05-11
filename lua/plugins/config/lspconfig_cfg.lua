local present1, _ = pcall(require, "lspconfig")
local present2, installer = pcall(require, "nvim-lsp-installer")
if not (present1 or present2) then
  vim.notify("Fail to setup LSP", vim.log.levels.ERROR, {
    title = "plugins",
  })
  return
end

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
  bmap("n", "gd", "<Cmd>Lspsaga preview_definition<CR>", opts)
  bmap("n", "gh", "<Cmd>Lspsaga hover_doc<CR>", opts)
  bmap("n", "<C-u>", "<cmd>lua require('lspsaga.action').smart_scroll_with_saga(-1)<cr>", opts)
  bmap("n", "<C-d>", "<cmd>lua require('lspsaga.action').smart_scroll_with_saga(1)<cr>", opts)
  bmap("n", "gs", "<cmd>Lspsaga signature_help<CR>", opts)
  bmap("n", "go", "<cmd>Lspsaga show_line_diagnostics<CR>", opts)
  bmap("n", "gj", "<cmd>Lspsaga diagnostic_jump_next<CR>", opts)
  bmap("n", "gk", "<cmd>Lspsaga diagnostic_jump_prev<CR>", opts)
  bmap("n", "gr", "<cmd>Lspsaga rename<CR>", opts)
  bmap("n", "ga", "<cmd>Lspsaga code_action<CR>", opts)

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

-- lspInstall + lspconfig stuff

-- provide analyse, completion for neovim runtime file
-- @return table
local function neovim_lua_setting()
  local runtime_path = vim.split(package.path, ";")
  table.insert(runtime_path, "lua/?.lua")
  table.insert(runtime_path, "lua/?/init.lua")
  return {
    Lua = {
      runtime = {
        -- Tell the language server which version of Lua you're using (most likely LuaJIT in the case of Neovim)
        version = "LuaJIT",
        -- Setup your lua path
        path = runtime_path,
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

if installer.settings then
  installer.settings({
    ui = {
      icons = {
        server_installed = "✓",
        server_pending = "➜",
        server_uninstalled = "✗",
      },
    },
  })

  local ensure_installed_server = {
    "sumneko_lua",
  }

  for _, lang in pairs(ensure_installed_server) do
    local ok, server = installer.get_server(lang)
    if ok then
      if not server:is_installed() then
        print("Installing " .. lang)
        server:install()
      end
    end
  end

  installer.on_server_ready(function(server)
    local opts = {
      on_attach = on_attach,
      capabilities = setup_capabilities(),
      root_dir = vim.loop.cwd,
    }

    if server.name == "sumneko_lua" then
      opts.settings = neovim_lua_setting()
    end

    -- This setup() function is exactly the same as lspconfig's setup function (:help lspconfig-quickstart)
    server:setup(opts)
    vim.cmd([[ do User LspAttachBuffers ]])
  end)
end

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
