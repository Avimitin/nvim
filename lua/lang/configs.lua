local config = {
  -- Told LSP server about the nvim lsp capabilities
  capabilities = require("cmp_nvim_lsp").default_capabilities(),
  -- Setup keymap on attach
  on_attach = require("lang.on_attach").setup_all,
  settings = {},
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
local _rust_find_config = function()
  local root_dir = require("libs.find_root").find_root({
    patterns = { ".rust-analyzer.json", "Cargo.toml", ".git" },
  })
  local cfg_file = vim.fs.joinpath(root_dir, ".rust-analyzer.json")

  -- prompt when try to read the file, to avoid security issue
  local file = vim.secure.read(cfg_file)
  if not file then
    return nil
  end

  local rust_settings = { ["rust-analyzer"] = vim.json.decode(file) }
  return rust_settings
end
setup("rust_analyzer", { settings = _rust_find_config() })
