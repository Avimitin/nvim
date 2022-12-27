local ok, error = pcall(require, "rust-tools")
if not ok then
  vim.notify(error)
  return
end

local on_lsp_attach = function(client, bufnr)
  -- setup lsp key mappings
  require("overlays.rc.lspconfig_keymap").lsp_keymap(client, bufnr)

  -- create auto command to format on save
  vim.api.nvim_create_autocmd({ "BufWrite" }, {
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
      ["rust-analyzer"] = vim.g.nvcfg.rust_config or {},
    },
    on_attach = on_lsp_attach,
  }, -- rust-analyer options
}

require("rust-tools").setup(opts)

vim.g.rustfmt_options = "--edition=2021"
