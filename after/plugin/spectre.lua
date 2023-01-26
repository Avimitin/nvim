if require("libs.g").spectre then
  return
end

vim.api.nvim_create_user_command("Sed", function()
  require("spectre").open()
end, {})
