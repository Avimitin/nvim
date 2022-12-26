if require("libs.g").flog then
  return
end

-- vim-flog is depend on vim-fugitive, however the vim-fugitive is lazy-loaded.
-- This script act as a middleware, to ensure vim-fugitive is loaded before vim-flog.

local opts = {
  range = 0,
  nargs = "*",
}

vim.api.nvim_create_user_command("Flog", function(args)
  require("packer").loader("vim-fugitive")
  require("packer").loader("vim-flog")

  if args.mods and args.mods ~= "" then
    vim.cmd(("%s Flog %s"):format(args.mods, args.args))
  else
    vim.cmd(("Flog %s"):format(args.args))
  end
end, opts)

vim.api.nvim_create_user_command("Flogsplit", function(args)
  require("packer").loader("vim-fugitive")
  require("packer").loader("vim-flog")
  if args.mods and args.mods ~= "" then
    vim.cmd(("%s Flogsplit %s"):format(args.mods, args.args))
  else
    vim.cmd(("Flogsplit %s"):format(args.args))
  end
end, opts)
