local new_cmd = require('utils').new_cmd
-- plugin neoclip
new_cmd("ClipRec", [[lua require('neoclip').start()]])
new_cmd("ClipView", [[Telescope neoclip]])

-- plugin lua formmater
new_cmd("LuaFormat", [[call LuaFormat()]])

-- plugin focus
new_cmd("FSplit", [[FocusSplitNicely]])

new_cmd("BufCL", [[BufferLineCloseLeft]])
new_cmd("BufCR", [[BufferLineCloseRight]])
