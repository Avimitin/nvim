local ok, error = pcall(require, "rust-tools")
if not ok then
  vim.notify(error)
  return
end

-- My default global setting for rust-analyzer.
-- You can create a `.rust-analyzer.json` file next to your `Cargo.toml` file to
-- add custom settings per project.
local rust_analyzer_settings = {
  cargo = {
    autoreload = true,
  },
  -- I would prefer to use cargo clippy to whip me more
  checkOnSave = {
    command = "clippy",
  },
}

local on_lsp_attach = function(client, bufnr)
  -- setup lsp key mappings
  require("plugins.coding.keymap").lsp_keymap(client, bufnr)

  -- create auto command to format on save
  vim.api.nvim_create_autocmd({ "BufWritePost" }, {
    buffer = bufnr,
    desc = "Format Rust code on save",
    callback = function()
      vim.lsp.buf.format({ async = true })
    end,
  })

  -- Setup key mapping
  local Hydra = require("hydra")

  local hint = [[
                   Rust Tools

  _r_:  Runnables         _c_:  Open Cargo.toml    
  _a_: ﯧ Hover Actions     _p_: פּ Goto parent module 
  _M_: ﬕ Expand Macro      _J_:  Join multiple line 

                   _q_: Exit
]]

  Hydra({
    name = "Rust Tools",
    hint = hint,
    config = {
      buffer = bufnr,
      invoke_on_body = true,
      color = "red",
      hint = {
        position = "bottom",
        border = "rounded",
      },
    },
    mode = "n",
    body = "<leader>r",
    heads = {
      {
        "r",
        function()
          require("rust-tools").runnables.runnables()
        end,
        { exit = true, desc = "Rust Runnables" },
      },
      {
        "a",
        function()
          require("rust-tools").hover_actions.hover_actions()
        end,
        { exit = true, desc = "Rust Hover Actions" },
      },
      {
        "c",
        function()
          require("rust-tools").open_cargo_toml.open_cargo_toml()
        end,
        { exit = true, desc = "Open Cargo.toml" },
      },
      {
        "p",
        function()
          require("rust-tools").parent_module.parent_module()
        end,
        { exit = true, desc = "Go to parent module" },
      },
      {
        "J",
        function()
          require("rust-tools").join_lines.join_lines()
        end,
        { exit = true, desc = "Join multiple line" },
      },
      {
        "M",
        function()
          require("rust-tools").expand_macro.expand_macro()
        end,
        { exit = true, desc = "Expand macro" },
      },
      { "q", nil, { exit = true, nowait = true, desc = "exit" } },
    },
  })
end

-- rust-tools.nvim settings
local opts = {
  tools = {
    executor = require("rust-tools/executors").termopen,

    -- These apply to the default RustSetInlayHints command
    inlay_hints = {
      auto = true,
      show_parameter_hints = true,
      parameter_hints_prefix = "<- ",
      other_hints_prefix = "=> ",
      max_len_align = false,
      max_len_align_padding = 1,
      right_align = false,
      right_align_padding = 7,
    },

    hover_actions = {
      auto_focus = true,
    },
  },
  -- send our rust-analyzer configuration to lspconfig
  server = {
    settings = {
      ["rust-analyzer"] = rust_analyzer_settings,
    },
    on_attach = on_lsp_attach,
  }, -- rust-analyer options
}

-- Since rust-tools.nvim is installed as plugin, we can ensure that
-- if rust-tools.nvim exist and invokes this configuration,
-- plenary and nvim-rooter should also exist in runtime path.

-- plenary plugin is a wrapper plugin for the libuv library.
-- It can gives us asynchronos operation on file.
local path_api = require("plenary.path")

-- json API is contained in our configuration
local json_api = require("libs.json")

-- nvim-rooter plugin can help us find project root directory.
local rooter_api = require("nvim-rooter")

-- if get_root API return nil, it means we are already inside root directory,
-- so just use current directory.
local project_root = rooter_api.get_root() or vim.fn.getcwd()

local function parse_custom_ra_config()
  local ra_json_file = path_api:new(project_root, ".rust-analyzer.json")
  local has_file, ra_json_content = pcall(ra_json_file.read, ra_json_file)
  if not has_file then
    -- silently return if no custom setting was found
    return nil
  end

  local parse_ok, setting = pcall(json_api.decode, ra_json_content)
  if not parse_ok then
    vim.notify(
      "Fail to parse .rust-analyer.json file, please check your config. Fallback to default."
    )
    return nil
  end

  return setting
end

-- insert user custom settings into default setting
local custom_settings = parse_custom_ra_config()
if custom_settings then
  rust_analyzer_settings = vim.tbl_deep_extend("force", rust_analyzer_settings, custom_settings)
  opts.server.settings["rust-analyzer"] = rust_analyzer_settings
end

require("rust-tools").setup(opts)

vim.g.rustfmt_options = "--edition=2021"
