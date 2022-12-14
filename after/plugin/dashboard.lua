if require("libs.g").dashboard then
  return
end

vim.api.nvim_create_autocmd("UIEnter", {
  group = vim.api.nvim_create_augroup("dashboard_cond_load", { clear = true }),
  nested = true,
  callback = function()
    if vim.fn.argc() == 0 and vim.fn.line2byte("$") == -1 then
      require("packer").loader("dashboard-nvim")
      require("overlays.rc.dashboard")
      require("dashboard"):instance(false)
    end
  end,
})
