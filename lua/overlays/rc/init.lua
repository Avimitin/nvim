--! This file is a configuration re-export file.
--! Please don't contains any blocking operation here.
--!
--! If field is only a function, then it will be considered as the config function.
--! If the plugin have some setup that should be execute before it loaded, return a
--! table like `{ config = function() ... end, setup = function() ... end }` and put
--! those setup step into the function for setup field.

local rc = {}

vim.g.matchup_matchparen_offscreen = {}

--
-- Simple re-export
--

local re_export = {
  "cmp",
  "null_ls",
  "treesitter",
  "lspconfig",
  "lspsaga",
  "rust",
  "symbols_outline",
  "bufferline",
  "galaxyline",
  "nvim_tree",
  "toggleterm",
  "telescope",
  "autopairs",
}

for _, mod in ipairs(re_export) do
  rc[mod] = function()
    require(string.format("overlays.rc.%s", mod))
  end
end

--
-- Config contains both config and setup
--

rc.dap = {
  config = function()
    require("overlays.rc.dap")
  end,
  setup = function()
    require("overlays.rc.dapcmds")
  end,
}

rc.crates = {
  config = function() end,
  setup = function() end,
}

rc.multi_cursor = {
  setup = function()
    -- visual multi mappings
    -- clean the keymap `u` and initialize the new keymap set
    require("editor.keymap").map("", "u", "<nop>")

    -- u is map to <C-z>, let us reuse it here
    vim.g.VM_maps = {
      ["Find Under"] = "\\n",
      ["Find Subword Under"] = "\\n",
      ["Select Cursor Down"] = "<C-down>",
      ["Select Cursor Up"] = "<C-up>",
      ["Select All"] = "uA",
      ["Undo"] = "<C-z>",
      ["Redo"] = "<C-r>",
      ["Start Regex Search"] = "ux",
      ["Visual Regex"] = "ux",
      ["Visual All"] = "uA",
      ["Visual Add"] = "ua",
      ["Visual Find"] = "uf",
      ["Visual Cursors"] = "uc",
    }
  end,
}

rc.wildfire = {
  setup = function()
    vim.g.wildfire_objects = { "i'", 'i"', "i)", "i]", "i}", "ip", "it", "i`" }
  end,
}

rc.dashboard = {
  setup = function()
    vim.api.nvim_create_autocmd("Vimenter", {
      group = vim.api.nvim_create_augroup("dashboard_cond_load", { clear = true }),
      nested = true,
      callback = function()
        if vim.fn.argc() == 0 and vim.fn.line2byte("$") == -1 then
          require("packer").loader("dashboard-nvim")
          require("overlays.rc.dashboard")
        end
      end,
    })
  end,
}

rc.rooter = {
  setup = function()
    vim.schedule(function()
      require("overlays.rc.rooter")
    end)
  end,
}

rc.theme = {
  kanagawa = function()
    require("overlays.rc.kanagawa")
  end,
  deus = function()
    vim.g.deus_background = "hard"
    vim.cmd("colorscheme deus")
  end,
  github = function()
    require("overlays.rc.github_theme")
  end,
}

return rc
