if vim.g.galaxyline_loaded ~= nil then
  return
end

vim.g.galaxyline_loaded = 1

local gl = require("galaxyline")
local gls = gl.section
local diagnostic = require("galaxyline.provider_diagnostic")

-- VistaPlugin = extension.vista_nearest

local current_scheme = vim.g.colors_name
local colors = {
  bg = "#2F3445",
  fg = "#8FBCBB",
  black = "#1F253A",
  yellow = "#E5C07B",
  cyan = "#70C0BA",
  dimblue = "#83A598",
  green = "#98C379",
  orange = "#FF8800",
  purple = "#C678DD",
  magenta = "#C858E9",
  blue = "#73BA9F",
  red = "#D54E53",
}

if current_scheme == "everforest" then
  colors.bg = "#282E2C"
  colors.black = "#222B28"
elseif current_scheme == "gruvbox" then
  colors.bg = "#261C00"
  colors.black = "#3A2300"
elseif current_scheme == "dawnfox" then
  colors.bg = "#898180"
  colors.black = "#625c5c"
elseif current_scheme:match("github_light[%l_]*") then
  local custom = {
    fg = "#24292f",
    bg = "#bbd6ee",
    black = "#9fc5e8",
    yellow = "#dbab09",
    cyan = "#0598bc",
    green = "#28a745",
    orange = "#d18616",
    magenta = "#5a32a3",
    purple = "#5a32a3",
    blue = "#0366d6",
    red = "#d73a49",
  }

  -- merge custom color to default
  colors = vim.tbl_deep_extend("force", {}, colors, custom)
end

local function has_file_type()
  local f_type = vim.bo.filetype
  if not f_type or f_type == "" then
    return false
  end
  return true
end

local buffer_not_empty = function()
  if vim.fn.empty(vim.fn.expand("%:t")) ~= 1 then
    return true
  end
  return false
end

-- insert_left insert item at the left panel
local function insert_left(element)
  table.insert(gls.left, element)
end

-- insert_blank_line_at_left insert blank line with
-- line_bg color.
local function insert_blank_line_at_left()
  insert_left({
    Space = {
      provider = function()
        return " "
      end,
      highlight = { colors.bg, colors.bg },
    },
  })
end

-- insert_right insert given item into galaxyline.right
local function insert_right(element)
  table.insert(gls.right, element)
end

-----------------------------------------------------
----------------- start insert ----------------------
-----------------------------------------------------
-- { mode panel start
insert_blank_line_at_left()

insert_left({
  ViMode = {
    icon = function()
      local icons = {
        n = " ",
        i = " ",
        c = "ﲵ ",
        V = " ",
        [""] = " ",
        v = " ",
        C = "ﲵ ",
        R = "﯒ ",
        t = " ",
      }
      return icons[vim.fn.mode()]
    end,
    provider = function()
      -- auto change color according the vim mode
      local alias = {
        n = "N",
        i = "I",
        c = "C",
        V = "VL",
        [""] = "V",
        v = "V",
        C = "C",
        ["r?"] = ":CONFIRM",
        rm = "--MORE",
        R = "R",
        Rv = "R&V",
        s = "S",
        S = "S",
        ["r"] = "HIT-ENTER",
        [""] = "SELECT",
        t = "T",
        ["!"] = "SH",
      }

      local mode_color = {
        n = colors.yellow,
        i = colors.green,
        v = colors.blue,
        [""] = colors.blue,
        V = colors.blue,
        c = colors.magenta,
        no = colors.red,
        s = colors.orange,
        S = colors.orange,
        [""] = colors.orange,
        ic = colors.yellow,
        R = colors.purple,
        Rv = colors.purple,
        cv = colors.red,
        ce = colors.red,
        r = colors.cyan,
        rm = colors.cyan,
        ["r?"] = colors.cyan,
        ["!"] = colors.red,
        t = colors.red,
      }

      local vim_mode = vim.fn.mode()
      vim.api.nvim_command("hi GalaxyViMode guifg=" .. mode_color[vim_mode])
      return alias[vim_mode]
    end,
    highlight = { colors.bg, colors.bg },
  },
})

insert_blank_line_at_left()

insert_left({
  EndingSepara = {
    provider = function()
      return " "
    end,
    highlight = { colors.bg },
  },
})

-- mode panel end}

-- {information panel start
insert_left({
  StartSeparate = {
    provider = function()
      return " "
    end,
    highlight = { colors.bg },
  },
})

insert_left({
  GitIcon = {
    provider = function()
      return "  "
    end,
    condition = require("galaxyline.provider_vcs").check_git_workspace,
    highlight = { colors.orange, colors.bg },
  },
})

insert_left({
  GitBranch = {
    provider = "GitBranch",
    condition = require("galaxyline.provider_vcs").check_git_workspace,
    highlight = { colors.fg, colors.bg },
  },
})

insert_blank_line_at_left()

local checkwidth = function()
  local squeeze_width = vim.fn.winwidth(0) / 2
  if squeeze_width > 50 then
    return true
  end
  return false
end

insert_left({
  DiffAdd = {
    provider = "DiffAdd",
    condition = checkwidth,
    icon = "  ",
    highlight = { colors.green, colors.bg },
  },
})

insert_left({
  DiffModified = {
    provider = "DiffModified",
    condition = checkwidth,
    icon = "  ",
    highlight = { colors.orange, colors.bg },
  },
})

insert_left({
  DiffRemove = {
    provider = "DiffRemove",
    condition = checkwidth,
    icon = "  ",
    highlight = { colors.red, colors.bg },
  },
})

local DiagnosticError = diagnostic.get_diagnostic_error
local DiagnosticWarn = diagnostic.get_diagnostic_warn
local DiagnosticHint = diagnostic.get_diagnostic_hint
local DiagnosticInfo = diagnostic.get_diagnostic_info

insert_left({
  DiagnosticError = {
    provider = DiagnosticError,
    icon = "  ",
    highlight = { colors.red, colors.bg },
  },
})

insert_left({
  DiagnosticWarn = {
    provider = DiagnosticWarn,
    condition = checkwidth,
    icon = "  ",
    highlight = { colors.yellow, colors.bg },
  },
})

insert_left({
  DiagnosticInfo = {
    provider = DiagnosticInfo,
    condition = checkwidth,
    highlight = { colors.green, colors.bg },
    icon = "  ",
  },
})

insert_left({
  DiagnosticHint = {
    provider = DiagnosticHint,
    condition = checkwidth,
    highlight = { colors.white, colors.bg },
    icon = "  ",
  },
})

insert_left({
  TriangleSeparate = {
    provider = function()
      return ""
    end,
    highlight = { colors.bg, colors.black },
  },
})

insert_left({
  BlackSpace = {
    provider = function()
      return " "
    end,
    highlight = { colors.black, colors.black },
  },
})

insert_left({
  FileIcon = {
    provider = "FileIcon",
    condition = buffer_not_empty,
    highlight = {
      require("galaxyline.provider_fileinfo").get_file_icon_color,
      colors.black,
    },
  },
})

insert_left({
  BufferType = {
    provider = "FileName",
    condition = has_file_type,
    highlight = { colors.fg, colors.black },
  },
})

insert_left({
  DarkSepara = {
    provider = function()
      return ""
    end,
    highlight = { colors.black },
  },
})
-- left information panel end}

insert_right({
  Start = {
    provider = function()
      return " "
    end,
    highlight = { colors.bg },
  },
})

insert_right({
  LineInfo = {
    provider = "LineColumn",
    separator = "  ",
    separator_highlight = { colors.green, colors.bg },
    highlight = { colors.fg, colors.bg },
  },
})

insert_right({
  RightEndingSepara = {
    provider = function()
      return ""
    end,
    highlight = { colors.bg, colors.black },
  },
})

insert_right({
  GetLspClient = {
    provider = "GetLspClient",
    separator = " LSP: ",
    separator_highlight = { colors.blue, colors.black },
    condition = checkwidth,
    highlight = { colors.fg, colors.black },
  },
})

insert_right({
  PerCent = {
    provider = "LinePercent",
    separator = " ",
    separator_highlight = { colors.blue, colors.black },
    condition = checkwidth,
    highlight = { colors.cyan, colors.black },
  },
})

insert_right({
  FileFormat = {
    provider = "FileFormat",
    separator = " ",
    condition = checkwidth,
    separator_highlight = { colors.blue, colors.black },
    highlight = { colors.fg, colors.black },
  },
})

insert_right({
  RightSpace = {
    provider = function()
      return " "
    end,
    highlight = { colors.black, colors.black },
  },
})

-- ============================= short line ===============================

gl.short_line_list = {
  "LuaTree",
  "vista",
  "dbui",
  "startify",
  "term",
  "nerdtree",
  "fugitive",
  "fugitiveblame",
  "plug",
  "NvimTree",
  "DiffviewFiles",
  "Outline",
  "neoterm",
  "fern",
  "toggleterm",
}

local BufferTypeMap = {
  ["Mundo"] = "Mundo History",
  ["MundoDiff"] = "Mundo Diff",
  ["NvimTree"] = " Tree",
  ["fugitive"] = " Fugitive",
  ["fugitiveblame"] = " Fugitive Blame",
  ["help"] = " Help",
  ["minimap"] = "Minimap",
  ["qf"] = " Quick Fix",
  ["tabman"] = "Tab Manager",
  ["tagbar"] = "Tagbar",
  ["FTerm"] = "Terminal",
  ["neoterm"] = " NeoTerm",
  ["toggleterm"] = " ToggleTerm",
  ["git"] = " Git",
  ["NeogitStatus"] = " Neogit Status",
  ["NeogitPopup"] = " Neogit Popup",
  ["NeogitCommitMessage"] = " Neogit Commit",
  ["DiffviewFiles"] = " Diff View",
  ["dapui_scopes"] = "כֿ Dap Scope",
  ["dapui_breakpoints"] = " Dap Breakpoints",
  ["dapui_stacks"] = " Dap Stacks",
  ["dapui_watches"] = "ﭓ Dap Watch",
  ["dap-repl"] = " Dap REPL",
  ["Outline"] = " SymbolOutline",
  ["fern"] = " Fern FM",
}

require("galaxyline").section.short_line_left = {
  {
    ShortLineLeftBufferType = {
      highlight = { colors.cyan, colors.bg },
      provider = function()
        local name = BufferTypeMap[vim.bo.filetype] or " Editor"
        return string.format("  %s", name)
      end,
      separator = " ",
      separator_highlight = { colors.bg, colors.black },
    },
  },
  {
    ShortLineLeftWindowNumber = {
      highlight = { colors.cyan, colors.black },
      provider = function()
        return " " .. vim.api.nvim_win_get_number(vim.api.nvim_get_current_win())
      end,
      separator = "",
      separator_highlight = { colors.black, "#1F253A" },
    },
  },
}
