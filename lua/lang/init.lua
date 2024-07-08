local register = require("pack").register

-- Interact with LSP server
register("neovim/nvim-lspconfig", {
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

    require("cmp").setup.buffer({
      sources = {
        { name = "crates" },
      },
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

register("scalameta/nvim-metals", {
  lazy = true,
})

register("stevearc/conform.nvim", {
  ft = {
    "lua",
    "javascript",
    "typescript",
    "javascriptreact",
    "typescriptreact",
    "json",
    "nix",
    "haskell",
    "python",
    "rust",
  },
  config = function()
    require("conform").setup({
      formatters_by_ft = {
        lua = { "stylua" },
        -- Use a sub-list to run only the first available formatter
        javascript = { { "prettierd", "prettier" } },
        json = { { "prettierd", "prettier" } },
        typescript = { { "prettierd", "prettier" } },
        nix = { "nixpkgs_fmt" },
        haskell = { "fourmolu" },
        python = { "black" },
        rust = { "rustfmt" },
      },
      format_on_save = function(bufnr)
        if vim.api.nvim_buf_line_count(bufnr) >= 5000 then
          return
        end

        if vim.g.disable_autoformat or vim.b[bufnr].disable_autoformat then
          return
        end

        return { timeout_ms = 500, lsp_fallback = true }
      end,
    })

    vim.keymap.set("n", "gf", require("conform").format, { desc = "[LSP] Format code" })

    vim.api.nvim_create_user_command("FormatDisable", function(args)
      if args.bang then
        -- FormatDisable! will disable formatting just for this buffer
        vim.b.disable_autoformat = true
      else
        vim.g.disable_autoformat = true
      end
    end, {
      desc = "Disable autoformat-on-save",
      bang = true,
    })
    vim.api.nvim_create_user_command("FormatEnable", function()
      vim.b.disable_autoformat = false
      vim.g.disable_autoformat = false
    end, {
      desc = "Re-enable autoformat-on-save",
    })
  end,
})

local export = {}

---@param server string Server name
---@param extra table Extra config to override the default
function export.run_lsp(bufnr, server, extra)
  local config = {
    -- Told LSP server about the nvim lsp capabilities
    capabilities = require("cmp_nvim_lsp").default_capabilities(),
    -- Setup keymap on attach
    on_attach = require("lang.on_attach").setup_all,
    settings = {},
  }

  if extra then
    config = vim.tbl_deep_extend("force", config, extra)
  end

  local lspconfig = require("lspconfig")

  lspconfig[server].setup(config)
  -- manually setup because FileType event is behind BufReadPost event
  lspconfig[server].manager:try_add_wrapper(bufnr, nil)
end

return export
