local nmap = require("libs.keymaps").nmap
nmap("n", [[<Cmd>execute('normal! ' . v:count1 . 'n')<CR><Cmd>lua require('hlslens').start()<CR>]])
nmap("N", [[<Cmd>execute('normal! ' . v:count1 . 'N')<CR><Cmd>lua require('hlslens').start()<CR>]])
nmap("*", [[*<Cmd>lua require('hlslens').start()<CR>]])
nmap("#", [[#<Cmd>lua require('hlslens').start()<CR>]])
nmap("g*", [[g*<Cmd>lua require('hlslens').start()<CR>]])
nmap("g#", [[g#<Cmd>lua require('hlslens').start()<CR>]])

require("scrollbar.handlers.search").setup()
