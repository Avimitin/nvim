vim.lsp.config("*", {
  -- Told LSP server about the nvim lsp capabilities
  capabilities = require("cmp_nvim_lsp").default_capabilities(),
  -- Setup keymap on attach
  on_attach = require("lang.on_attach").setup_all,
})

-- C/CPP
vim.lsp.config("ccls", {
  root_dir = require("lspconfig").util.root_pattern(
    "build",
    "compile_commands.json",
    ".ccls",
    ".git"
  ),
  init_options = {
    compilationDatabaseDirectory = "build",
    index = {
      threads = 0,
    },
    clang = {
      excludeArgs = { "-frounding-math" },
    },
  },
})

-- Haskell
vim.lsp.enable("hls")
vim.lsp.config("hls", {
  settings = {
    haskell = {
      formattingProvider = "fourmolu",
      plugin = {
        rename = {
          config = {
            crossModule = true,
          },
        },
      },
    },
  },
})

-- Lua
local _lua_config = {}
local _is_nvim_config_dir = (vim.fn.getcwd()):find("nvim")
if _is_nvim_config_dir then
  local neovim_setting = {
    diagnostics = {
      enable = true,
      globals = { "vim" },
    },
    runtime = {
      version = "LuaJIT",
      path = vim.split(package.path, ";"),
    },
    workspace = {
      library = {
        vim.env.VIMRUNTIME,
        vim.api.nvim_get_runtime_file("", true),
      },
      checkThirdParty = false,
    },
    completion = {
      callSnippet = "Replace",
    },
  }
  _lua_config.settings = {
    Lua = neovim_setting,
  }
end
vim.lsp.config("lua_ls", _lua_config)

-- Rust
vim.lsp.config("rust_analyzer", {
  on_init = function(client)
    local _, folder = next(client.workspace_folders)
    if folder == nil then
      return true
    end

    local current_dir = folder.name

    local cfg_file = vim.fs.joinpath(current_dir, ".rust-analyzer.json")

    -- prompt when try to read the file, to avoid security issue
    local file = vim.secure.read(cfg_file)
    if not file then
      return true
    end

    local local_settings = { ["rust-analyzer"] = vim.json.decode(file) }
    client.config.settings = vim.tbl_deep_extend("force", client.config.settings, local_settings)
    client.notify("workspace/didChangeConfiguration", { settings = client.config.settings })

    vim.notify(
      "Using new local settings in " .. vim.fs.normalize(current_dir .. "/.rust-analyzer.json")
    )

    return true
  end,
})

vim.lsp.config("tinymist", {
  settings = {
    formatterMode = "typstyle",
    exportPdf = "onType",
    semanticTokens = "disable",
  },
})

local enable_servers = {
  "ccls",
  "hls",
  "ts_ls",
  "lua_ls",
  "nil_ls",
  "ocamllsp",
  "pyright",
  "rust_analyzer",
  "tinymist",
}
for _, server in ipairs(enable_servers) do
  vim.lsp.enable(server)
end
