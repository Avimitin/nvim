local register = require("pack").register

-- Interact with LSP server
register("neovim/nvim-lspconfig", {
  lazy = true,
  config = function()
    require("lsp.icons").setup()
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
  config = function()
    require("crates").setup({
      popup = {
        autofocus = true,
        border = "single",
      },
    })

    -- Find buffer id for `Cargo.toml` file
    local existing_buffer = vim.api.nvim_list_bufs()
    local cargo_toml_buf_id = nil
    for _, buf_id in ipairs(existing_buffer) do
      local buf_name = vim.api.nvim_buf_get_name(buf_id)
      local filename = vim.fn.fnamemodify(buf_name, ":t")
      if filename == "Cargo.toml" then
        cargo_toml_buf_id = buf_id
      end
    end
    if cargo_toml_buf_id == nil then
      return
    end

    local crates = require("crates")

    -- this key mappings will only apply to the `Cargo.toml` file buffer
    require("libs.keymap").buf_map(cargo_toml_buf_id, "n", {
      { "<leader>cu", crates.upgrade_crate, desc = "Upgrade crate under current cursor" },
      { "<leader>cv", crates.show_versions_popup, desc = "Show current crate versions" },
      { "<leader>cf", crates.show_features_popup, desc = "Show current crate features" },
      { "<leader>cR", crates.open_repository, desc = "Open source code in browser" },
      { "<leader>cD", crates.open_documentation, desc = "Open docs.rs in browser" },
    })
  end,
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

---@param server string Server name
---@param extra table Extra config to override the default
function export.start(server, extra)
  local config = require("lsp.config")
  if extra then
    -- This value might be nil, so we need to assign default values
    config.on_attach = extra.on_attach or require("lsp.keymaps")
    config.settings = extra.settings or {}

    -- And finally try to merge other settings
    config = vim.tbl_deep_extend("force", config, extra)
  end

  local lspconfig = require("lspconfig")

  lspconfig[server].setup(config)
  -- manually setup because FileType event is behind BufReadPost event
  lspconfig[server].manager.try_add_wrapper()
end

return export
