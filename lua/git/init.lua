local pack = require("pack").register

pack("TimUntersberger/neogit", {
  config = function()
    require("neogit").setup({})
  end,
  keys = {
    {
      "<leader>gn",
      function()
        require("neogit").open()
      end,
      desc = "Open neogit",
    },
  },
})

pack("lewis6991/gitsigns.nvim", {
  lazy = true,
  init = function()
    vim.api.nvim_create_autocmd({ "BufAdd", "VimEnter" }, {
      callback = function()
        local function callback(code, _)
          if code == 0 then
            vim.schedule(function()
              require("git.gitsigns")
              require("scrollbar.handlers.gitsigns").setup()
            end)
          end
        end

        vim.loop.spawn("git", {
          args = {
            "ls-files",
            "--error-unmatch",
            vim.fn.expand("%"),
          },
        }, callback)
      end,
    })
  end,
})
