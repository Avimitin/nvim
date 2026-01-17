local register = require("pack").register

register("hrsh7th/nvim-cmp", {
  rev = "85bbfad83f804f11688d1ab9486b459e699292d6",
  sha256 = "sha256-gwuiUgz3UEFpaKs79BSWS4qkwOi+XMHIDFdYRatWt0g=",
  event = "InsertEnter",
  keys = { ":", "/" },
  dependencies = {
    -- from lsp
    "hrsh7th/cmp-nvim-lsp",
    -- for function parameters
    "hrsh7th/cmp-nvim-lsp-signature-help",
    -- for OS path
    "hrsh7th/cmp-path",
    -- for vim command line
    "hrsh7th/cmp-cmdline",
  },

  config = function()
    require("completion.rc")
  end,
})
