-- load config that should be initialize before plugins
require("plugins.completion.config").pre()

-- register plugin repos
local repo = require("plugins.completion.repos")
require("plugins").register(repo)
