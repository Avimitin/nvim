-- key mappings
require("plugins.enhance.keymap")

-- config before plugins
require("plugins.enhance.config").pre()

-- register plugin repos
local repo = require("plugins.enhance.repos")
require("plugins").register(repo)

-- setup commands
require("plugins.enhance.commands")

-- setup auto commands
require("plugins.enhance.autocmd")
