local ok, error = pcall(require, "rust-tools")
if not ok then
  vim.notify(error)
  return
end

local on_lsp_attach = function(client, bufnr)
  -- setup lsp key mappings
  require("lsp.keymaps")(client, bufnr)

  -- create auto command to format on save
  vim.api.nvim_create_autocmd({ "BufWrite" }, {
    buffer = bufnr,
    desc = "Format Rust code on save",
    callback = function()
      vim.lsp.buf.format({ async = true })
    end,
  })

  -- TODO: Add Rust key mappings
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
      ["rust-analyzer"] = vim.cfg.rust.settings,
    },
    on_attach = on_lsp_attach,
  }, -- rust-analyer options
}

require("rust-tools").setup(opts)
require("lspconfig")["rust_analyzer"].manager.try_add_wrapper()
