return {
  -- A git tool like magit in Emacs
  {
    'tpope/vim-fugitive',
    cmd = {
      'G', 'Git',
      'Ggrep',
      'Gdiffsplit',
      'GBrowse'
    }
  },

  -- Call the lazygit inside neovim, relies on lazygit executable
  {
    'kdheepak/lazygit.nvim',
    setup = function()
      vim.g.lazygit_floating_window_winblend = 0
      vim.g.lazygit_floating_window_scaling_factor = 1
      vim.g.lazygit_floating_window_corner_chars = {
        '╭',
        '╮',
        '╰', '╯'
      }
      vim.g.lazygit_floating_window_use_plenary = 0
      vim.g.lazygit_use_neovim_remote = 1
      if vim.g.lazygit_use_neovim_remote == 1 and vim.fn.executable('nvr') then
        vim.env.GIT_EDITOR = "nvr -cc split --remote-wait +'set bufhidden=wipe'"
      end
    end,
    cmd = "LazyGit"
  },

  -- Show git information in neovim
  {
    'lewis6991/gitsigns.nvim',
    requires = {
      'nvim-lua/plenary.nvim'
    },
    event = "BufRead",
    config = function()
      require("config.gitsign")
    end
  },

  {
    'sindrets/diffview.nvim',
    requires = 'nvim-lua/plenary.nvim',
    config = function ()
      require("diffview").setup({
        -- see configuration in
        -- https://github.com/sindrets/diffview.nvim#configuration
      })
    end,
    cmd = {
      "DiffviewOpen",
      "DiffviewFileHistory"
    }
  },

  {
    'rbong/vim-flog',
    -- run command after the vim fugitive
    cmd = {"Flog", "Flogsplit"},
  }
}
