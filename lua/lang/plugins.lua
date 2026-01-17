local register = require("pack").register

register("numToStr/Comment.nvim", {
  rev = "e30b7f2008e52442154b66f7c519bfd2f1e32acb",
  sha256 = "sha256-h0kPue5Eqd5aeu4VoLH45pF0DmWWo1d8SnLICSQ63zc=",
  config = function()
    require("Comment").setup({})
  end,
})

register("scalameta/nvim-metals", {
  rev = "7ed47cd3095508e80423e30922385d5b9988f9d6",
  sha256 = "sha256-LhVcJ5KbQeUik1+ZEuY/zoLKXkLAYb+sn6EcJFMfLjw=",
})

register("stevearc/conform.nvim", {
  rev = "5ac2bb57a9096f00ca50e1a3c46020d5930319b8",
  sha256 = "sha256-YQivyPuskDJO4qK55VgOHXRsB4y/shA9T44iyfoNG2k=",
  config = function()
    require("conform").setup({
      formatters_by_ft = {
        lua = { "stylua" },
        javascript = { "prettierd", "prettier", stop_after_first = true },
        json = { "prettierd", "prettier", stop_after_first = true },
        typescript = { "prettierd", "prettier", stop_after_first = true },
        nix = { "nixfmt" },
        haskell = { "fourmolu" },
        python = { "ruff_format" },
        rust = { "rustfmt" },
        ocaml = { "ocamlformat" },
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

    require("keys").map("n", {
      "<leader>cf",
      function()
        -- Use LSP client provided formatter when no formatter specify
        require("conform").format({ lsp_format = "fallback" })
      end,
      desc = "[LSP] Format code",
    })

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
