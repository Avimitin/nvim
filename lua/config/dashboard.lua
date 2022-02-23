vim.g.dashboard_disable_statusline = 1
vim.g.dashboard_default_executive = "telescope"
vim.g.dashboard_custom_header = {
  [[      ___                                    ___     ]],
  [[     /__/\          ___        ___          /__/\    ]],
  [[     \  \:\        /__/\      /  /\        |  |::\   ]],
  [[      \  \:\       \  \:\    /  /:/        |  |:|:\  ]],
  [[  _____\__\:\       \  \:\  /__/::\      __|__|:|\:\ ]],
  [[ /__/::::::::\  ___  \__\:\ \__\/\:\__  /__/::::| \:\]],
  [[ \  \:\~~\~~\/ /__/\ |  |:|    \  \:\/\ \  \:\~~\__\/]],
  [[  \  \:\  ~~~  \  \:\|  |:|     \__\::/  \  \:\      ]],
  [[   \  \:\       \  \:\__|:|     /__/:/    \  \:\     ]],
  [[    \  \:\       \__\::::/      \__\/      \  \:\    ]],
  [[     \__\/           ~~~~                   \__\/    ]],
}

vim.cmd([[
autocmd FileType dashboard set showtabline=0 laststatus=0
autocmd WinLeave <buffer> set showtabline=2 laststatus=2
]])
