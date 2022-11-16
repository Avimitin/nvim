local repos = {
  -- A git tool like magit in Emacs
  {
    "tpope/vim-fugitive",
    cmd = {
      "G",
      "Git",
      "Ggrep",
      "Gdiffsplit",
      "GBrowse",
    },
  },

  -- Show git information in neovim
  {
    "lewis6991/gitsigns.nvim",
    opt = true,
    setup = function()
      vim.api.nvim_create_autocmd({ "BufAdd", "VimEnter" }, {
        callback = function()
          local function onexit(code, _)
            if code == 0 then
              vim.schedule(function()
                require("packer").loader("gitsigns.nvim")
                require("plugins.git.config").gitsigns_config()
                require("scrollbar.handlers.gitsigns").setup()
              end)
            end
          end

          local lines = vim.api.nvim_buf_get_lines(0, 0, -1, false)
          if lines ~= { "" } then
            vim.loop.spawn("git", {
              args = {
                "ls-files",
                "--error-unmatch",
                vim.fn.expand("%"),
              },
            }, onexit)
          end
        end,
      })
    end,
  },

  -- Single tabpage interface for easily cycling through diffs for all modified files for any git rev.
  {
    "sindrets/diffview.nvim",
    requires = "nvim-lua/plenary.nvim",
    config = function()
      require("diffview").setup({
        -- see configuration in
        -- https://github.com/sindrets/diffview.nvim#configuration
      })
    end,
    cmd = {
      "DiffviewOpen",
      "DiffviewFileHistory",
    },
  },
}

return repos
