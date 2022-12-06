require("crates").setup({
  popup = {
    autofocus = true,
    border = "single",
  },
})

local Hydra = require("hydra")
local crates = require("crates")

local hint = [[
                                    Crates.nvim

       _u_: upgrade crates                          _U_: upgrade all crates
    _v_: Show versions        _f_: Show features        _d_: Show dependencies
 _O_: open homepage   _R_: open repository   _D_: open docs.rs   _C_: open crates.io

 _q_: exit
]]

Hydra({
  name = "Crates",
  hint = hint,
  config = {
    color = "pink",
    invoke_on_body = true,
    hint = {
      border = "rounded",
    },
  },
  mode = "n" ,
  body = "<Space>c",
  heads = {
    { "q", nil, { exit = true, nowait = true, desc = "exit" } },
    { "u", crates.upgrade_crate },
    { "U", crates.upgrade_all_crates, { exit = true, nowait = true } },
    { "v", crates.show_versions_popup, { exit = true, nowait = true } },
    { "f", crates.show_features_popup, { exit = true, nowait = true } },
    { "d", crates.show_dependencies_popup, { exit = true, nowait = true } },
    { "O", crates.open_homepage, { exit = true, nowait = true } },
    { "R", crates.open_repository, { exit = true, nowait = true } },
    { "D", crates.open_documentation, { exit = true, nowait = true } },
    { "C", crates.open_crates_io, { exit = true, nowait = true } },
  },
})
