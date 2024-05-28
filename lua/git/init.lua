local pack = require("pack").register

pack("lewis6991/gitsigns.nvim", {
  lazy = true,
  init = function()
    vim.api.nvim_create_autocmd({ "BufAdd", "BufEnter", "VimEnter" }, {
      callback = function()
        if vim.b._gitsign_setup then
          return
        end
        vim.b._gitsign_setup = true

        local function callback(code, _)
          if code == 0 then
            vim.schedule(function()
              require("git.gitsigns")
            end)
          end
        end

        vim.uv.spawn("git", {
          args = {
            "rev-parse",
            vim.fn.expand("%:p:h"),
          },
        }, callback)
      end,
    })
  end,
})
