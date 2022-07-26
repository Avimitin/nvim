-- initialize key mappings
require("plugins.git.keymap")

-- register plugin repos
local repos = require("plugins.git.repos")
require("plugins").register(repos)

require("plugins.git.autocmd").setup()
