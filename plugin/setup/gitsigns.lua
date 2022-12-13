if require("libs.g").gitsigns then
  return
end

vim.api.nvim_create_autocmd({ "BufAdd", "VimEnter" }, {
  callback = function()
    local function onexit(code, _)
      if code == 0 then
        vim.schedule(function()
          require("packer").loader("gitsigns.nvim")
          require("overlays.rc.gitsigns")
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
    }, onexit)
  end,
})
