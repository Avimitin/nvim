local ok, error = pcall(require, "rust-tools")
if not ok then
  vim.notify(error)
  return
end

local opts = {
  tools = {
    autoSetHints = true,
    hover_with_actions = true,
    executor = require("rust-tools/executors").termopen,
    runnables = {
      use_telescope = true,

      prompt_prefix = "  ",
      selection_caret = "  ",
      entry_prefix = "  ",
      initial_mode = "insert",
      selection_strategy = "reset",
      sorting_strategy = "descending",
      layout_strategy = "vertical",
      layout_config = {
        width = 0.3,
        height = 0.50,
        preview_cutoff = 0,
        prompt_position = "bottom",
      },
    },

    -- These apply to the default RustSetInlayHints command
    inlay_hints = {
      show_parameter_hints = true,
      show_variable_name = true,
      parameter_hints_prefix = "<- ",
      other_hints_prefix = "=> ",
      max_len_align = false,
      max_len_align_padding = 1,
      right_align = false,
      right_align_padding = 7,
    },

    hover_actions = {
      border = {
        { "╭", "FloatBorder" },
        { "─", "FloatBorder" },
        { "╮", "FloatBorder" },
        { "│", "FloatBorder" },
        { "╯", "FloatBorder" },
        { "─", "FloatBorder" },
        { "╰", "FloatBorder" },
        { "│", "FloatBorder" },
      },
      auto_focus = true,
    },
  },
  server = { on_attach = require("plugins.config.lspconfig_cfg").set_lsp_key }, -- rust-analyer options
}

require("rust-tools").setup(opts)

vim.g.rustfmt_options = "--edition=2021"
