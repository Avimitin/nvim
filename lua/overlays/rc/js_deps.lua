require("package-info").setup()

local function setup_hydra(bufnr)
  local Hydra = require("hydra")
  local pkginfo = require("package-info")

  local hint = [[
         packages
    --------------------
  _U_: Update dependencies
  _D_: Delete dependencies
  _I_: Install dependencies  
  _C_: Change version
  _S_: Show versions

   _<ESC>_/_q_: exit
  ]]

  Hydra({
    name = "Crates",
    hint = hint,
    buffer = bufnr,
    config = {
      color = "pink",
      invoke_on_body = true,
      hint = {
        border = "rounded",
        position = "bottom-right",
      },
      timeout = 2000, -- close after 2secs of inactive
    },
    mode = "n",
    body = "<Space>c",
    heads = {
      { "q", nil, { exit = true, nowait = true, desc = "exit" } },
      { "<ESC>", nil, { exit = true, nowait = true, desc = "exit" } },
      { "U", pkginfo.update, { silent = true } },
      { "D", pkginfo.delete, { silent = true } },
      { "I", pkginfo.install, { silent = true } },
      { "C", pkginfo.change_version, { silent = true } },
      { "S", pkginfo.toggle, { silent = true } },
    },
  })

  -- The UI should be setup when the code execute to here. But we still
  -- need a 1sec timer to defer the notification to avoid machine performance
  -- causing issue.
  -- TODO: Add VSCode like "Do not remind me" options
  vim.defer_fn(function()
    vim.notify("[package] Press <Space>c to open quick actions")
  end, 1000)
end

return {
  setup_hydra = setup_hydra,
}
