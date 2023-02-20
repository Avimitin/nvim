local register = require("pack").register

-- Auto-pairs key mappings
register("hrsh7th/nvim-insx", {
  event = "InsertEnter",
  branch = "main",
  config = function()
    require("insx.preset.standard").setup()
  end,
})

-- Neovim API completion sources
register("ii14/emmylua-nvim", {
  lazy = true,
})

-- Neovim Library wrapper
register("nvim-lua/plenary.nvim", {
  lazy = true,
})

-- Tree-like file browser
register("kyazdani42/nvim-tree.lua", {
  lazy = true,
  keys = {
    {
      "<leader>t",
      function()
        if vim.bo.filetype == "NvimTree" then
          require("nvim-tree").toggle()
        else
          require("nvim-tree").focus()
        end
      end,
    },
  },
  --
  init = function()
    -- Auto load nvim-tree when neovim is started with directory
    vim.api.nvim_create_autocmd("UIEnter", {
      pattern = "*",
      callback = function()
        if vim.fn.argc() == 0 then
          return
        end
        local first_arg = vim.fn.argv(0)
        if not first_arg or #first_arg == 0 then
          return
        end

        vim.loop.fs_stat(
          first_arg,
          vim.schedule_wrap(function(err, stat)
            if err then
              return
            end

            if stat.type ~= "directory" then
              return
            end

            require("nvim-tree").open(first_arg)
          end)
        )
      end,
    })
  end,
  --
  config = function()
    require("tools.nvim_tree")
  end,
}) -- End of nvim-tree
