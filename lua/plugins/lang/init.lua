return {
  -- Interact with LSP server
  {
    "neovim/nvim-lspconfig",
    lazy = true,
    config = function()
      require("plugins.lang.icons").setup()
    end,
  },

  -- UI for builtin LSP function
  {
    "glepnir/lspsaga.nvim",
    event = "LspAttach",
    cmd = "LspSaga",
    config = function()
      require("plugins.lang.lspsaga")
    end,
  },

  -- Inject more LSP sources
  {
    "jose-elias-alvarez/null-ls.nvim",
    lazy = true,
  },

  -- Rust specific plugin
  {
    "simrat39/rust-tools.nvim",
    lazy = true,
  },

  -- Cargo.toml manager
  {
    "saecki/crates.nvim",
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
        {
          "<leader>cu",
          crates.upgrade_crate,
          desc = "Upgrade crate under current cursor",
        },
        { "<leader>cv", crates.show_versions_popup, desc = "Show current crate versions" },
        { "<leader>cf", crates.show_features_popup, desc = "Show current crate features" },
        { "<leader>cR", crates.open_repository, desc = "Open source code in browser" },
        { "<leader>cD", crates.open_documentation, desc = "Open docs.rs in browser" },
        { "ga", "<CMD>Lspsaga code_action<CR>", desc = "Open actions" },
      })

      local whichkey = require("which-key")

      local ngrp = {
        mode = "n",
        buffer = cargo_toml_buf_id,
        ["<leader>c"] = { name = "+Crates" },
      }
      whichkey.register(ngrp)
    end,
  },

  {
    "numToStr/Comment.nvim",
    config = function()
      require("Comment").setup({})
    end,
    keys = {
      "gcc",
      "gbc",
      { mode = "x", "gc" },
      { mode = "x", "gb" },
    },
  },

  {
    "scalameta/nvim-metals",
    lazy = true,
  },
}
