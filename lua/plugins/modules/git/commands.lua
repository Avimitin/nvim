local alias = vim.api.nvim_create_user_command

alias("Glog", function()
  -- It will check if the vim-fugitive is loaded, so don't worry
  require("packer").loader("vim-fugitive")
  require("packer").loader("vim-flog")
  vim.cmd("Flog")
end)

alias("GlogS", function()
  -- It will check if the vim-fugitive is loaded, so don't worry
  require("packer").loader("vim-fugitive")
  require("packer").loader("vim-flog")
  vim.cmd("Flogsplit")
end)
