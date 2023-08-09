return {
  {
    "lewis6991/gitsigns.nvim",

    lazy = true,
    init = function()
      vim.api.nvim_create_autocmd({ "BufAdd", "VimEnter" }, {
        callback = function()
          local function callback(code, _)
            if code == 0 then
              vim.schedule(function()
                require("plugins.git.gitsigns")
                require("scrollbar.handlers.gitsigns").setup()
              end)
            end
          end

          vim.uv.spawn("git", {
            args = {
              "ls-files",
              "--error-unmatch",
              vim.fn.expand("%"),
            },
          }, callback)
        end,
      })
    end,
  },
}
