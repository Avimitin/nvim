-- Reject loading plugins when bigfile detect. Default on 1.5M size.
require("libs.bigfile").setup()

require("core")
require("pack").setup()
require("key-mapping")

vim.cmd.colorscheme("kanagawa")
