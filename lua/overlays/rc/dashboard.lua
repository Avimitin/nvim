local db = require("dashboard")
db.custom_header = {
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
require("telescope")
db.custom_center = {
  {
    icon = "  ",
    desc = "Create new file       ",
    action = "DashboardNewFile",
  },
  {
    icon = "﴾  ",
    desc = "Update Plugin         ",
    action = "PackerSync",
  },
  {
    icon = "  ",
    desc = "Recently opened files ",
    action = "Telescope oldfiles",
  },
  {
    icon = "  ",
    desc = "Find  File            ",
    action = "Telescope find_files find_command=rg,--hidden,--files",
  },
  {
    icon = "  ",
    desc = "Find  word            ",
    action = "Telescope live_grep",
  },
}
