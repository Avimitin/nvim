local register = require("pack").register

-- Interact with LSP server
register("neovim/nvim-lspconfig", {
  config = function()
    require("lang.configs")
  end,
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
    { "gcc", desc = "Toggle line comment" },
    { "gbc", desc = "Toggle block comment" },
    { mode = "x", "gc", desc = "Toggle line comment" },
    { mode = "x", "gb", desc = "Toggle block comment" },
  },
})

register("scalameta/nvim-metals", {
  ft = { "scala" },
  config = function()
    require("lang.scala")
  end,
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
    "scala",
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
        -- Avoid text lock for long time
        if vim.api.nvim_buf_line_count(bufnr) >= 5000 then
          return
        end

        if vim.g.disable_autoformat or vim.b[bufnr].disable_autoformat then
          return
        end

        return { timeout_ms = 500, lsp_fallback = true }
      end,
    })

    vim.keymap.set("n", "<leader>cf", function()
      -- Use LSP client provided formatter when no formatter specify
      require("conform").format({ lsp_format = "fallback" })
    end, { desc = "[LSP] Format code" })

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
