-- setup configuration
require("plugins.markdown.config").pre()

-- register plugin repos
local repo = require("plugins.markdown.repos")
require("plugins").register(repo)
