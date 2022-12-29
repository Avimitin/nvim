if require("libs.g")["nvim_tree"] then
  return
end

require("libs.keymaps").nmap("<leader>t", ":NvimTreeToggle<CR>")

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
