local config = {
  -- Told LSP server about the nvim lsp capabilities
  capabilities = require("cmp_nvim_lsp").default_capabilities(),
  -- Setup keymap on attach
  on_attach = require("lang.on_attach").setup_all,
  --settings = {},
}

local lspconfig = require("lspconfig")
local setup = function(server, settings)
  if settings then
    config = vim.tbl_deep_extend("force", config, settings)
  end

  lspconfig[server].setup(config)
end

-- C/CPP
setup("ccls", {
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
setup("hls", {
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

-- JavaScript/TypeScript
setup("ts_ls")
-- I don't use deno for now, but keep config here
-- setup("denols", require("lang.deno"))

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
setup("lua_ls", _lua_config)

-- Nix
setup("nil_ls")

-- OCaml
setup("ocamllsp")

-- Python
setup("pyright")

-- Rust
setup("rust_analyzer", {
  -- Don't know why built-in not working
  root_dir = function()
    return require("libs.find_root").find_root({ patterns = { "Cargo.toml" } })
  end,
  on_init = function(client)
    local _, folder = next(client.workspace_folders)
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

setup("tinymist", {
  settings = {
    formatterMode = "typstyle",
    exportPdf = "onType",
    semanticTokens = "disable",
  },
})
