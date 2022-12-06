--! This file must contains non-blocking setup functions

local setups_sets = {
  ["dap"] = function()
    require("overlays.rc.dapcmds")
  end,
  ["multi_cursor"] = function()
    -- visual multi mappings
    -- clean the keymap `u` and initialize the new keymap set
    require("libs.keymaps").map("", "u", "<nop>")

    -- u is map to <C-z>, let us reuse it here
    vim.g.VM_maps = {
      ["Find Under"] = "un",
      ["Find Subword Under"] = "un",
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
  ["wildfire"] = function()
    vim.g.wildfire_objects = { "i'", 'i"', "i)", "i]", "i}", "ip", "it", "i`" }
  end,
  ["dashboard"] = function()
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
  ["rooter"] = function()
    vim.schedule(function()
      require("overlays.rc.find_root")
    end)
  end,
  ["matchup"] = function()
    vim.g.matchup_matchparen_offscreen = {}
  end,
  ["crates"] = function()
    vim.api.nvim_create_autocmd("BufRead", {
      group = vim.api.nvim_create_augroup("CmpSourceCargo", { clear = true }),
      pattern = "Cargo.toml",
      callback = function()
        require("cmp").setup.buffer({ sources = { { name = "crates" } } })
      end,
    })
  end,
}

return setups_sets
