local gl = require("galaxyline")
local gls = gl.section

local current_scheme = vim.g.colors_name
local colors = {
  bg = "#0d0d0d",
  fg = "#b2b2b9",
  black = "#191919",
  yellow = "#E5C07B",
  cyan = "#70C0BA",
  dimblue = "#83A598",
  green = "#98C379",
  orange = "#FF8800",
  purple = "#C678DD",
  magenta = "#D27E99",
  blue = "#81A1C1",
  red = "#D54E53",
  divider = "#24242e",
}

if current_scheme == "everforest" then
  colors.bg = "#282E2C"
  colors.black = "#222B28"
elseif current_scheme == "gruvbox" then
  colors.bg = "#261C00"
  colors.black = "#3A2300"
  colors.divider = "#322e2e"
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

local checkwidth = function()
  local squeeze_width = vim.fn.winwidth(0) / 2
  if squeeze_width > 50 then
    return true
  end
  return false
end

local function should_activate_lsp()
  local clients = vim.lsp.get_active_clients()
  return checkwidth() and #clients ~= 0
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

-- insert_right insert given item into galaxyline.right
local function insert_right(element)
  table.insert(gls.right, element)
end

-----------------------------------------------------
----------------- start insert ----------------------
-----------------------------------------------------
-- { mode panel start
insert_left({
  ViMode = {
    provider = function()
      return " "
    end,
    highlight = { colors.black, colors.black },
  },
})

insert_left({
  ViModeIcon = {
    provider = function()
      -- auto change color according the vim mode
      local alias = {
        n = "",
        i = "",
        c = "",
        V = "",
        [""] = "",
        v = "",
        C = "",
        ["r?"] = "?",
        rm = "M",
        R = "",
        Rv = "",
        s = "",
        S = "",
        ["r"] = "HIT-ENTER",
        [""] = "",
        t = "T",
        ["!"] = "",
      }

      local mode = vim.fn.mode()
      local vim_mode_color = {
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

      vim.api.nvim_set_hl(0, "GalaxyViMode", { fg = vim_mode_color[mode], bg = colors.bg })
      return alias[mode]
    end,
    highlight = "GalaxyViMode",
  },
})

insert_left({
  EndingSepara = {
    provider = function()
      return " "
    end,
    highlight = function()
      if should_activate_lsp() then
        return { colors.bg, colors.black }
      else
        return { colors.bg, colors.divider }
      end
    end,
  },
})

insert_left({
  FileIcon = {
    provider = "FileIcon",
    condition = function()
      return buffer_not_empty() and should_activate_lsp()
    end,
    highlight = {
      require("galaxyline.provider_fileinfo").get_file_icon_color,
      colors.black,
    },
  },
})

insert_left({
  GetLspClient = {
    provider = "GetLspClient",
    condition = should_activate_lsp,
    highlight = { colors.fg, colors.black },
  },
})

insert_left({
  LspSpace = {
    provider = function()
      return " "
    end,
    condition = should_activate_lsp,
    highlight = { colors.black, colors.black },
  },
})

insert_left({
  DiagnosticError = {
    provider = "DiagnosticError",
    condition = should_activate_lsp,
    icon = "  ",
    highlight = { colors.red, colors.black },
  },
})

insert_left({
  DiagnosticWarn = {
    provider = "DiagnosticWarn",
    condition = should_activate_lsp,
    icon = "  ",
    highlight = { colors.yellow, colors.black },
  },
})

insert_left({
  DiagnosticInfo = {
    provider = "DiagnosticInfo",
    condition = should_activate_lsp,
    highlight = { colors.green, colors.black },
    icon = "  ",
  },
})

insert_left({
  DiagnosticHint = {
    provider = "DiagnosticHint",
    condition = should_activate_lsp,
    highlight = { colors.white, colors.black },
    icon = "  ",
  },
})

insert_left({
  LeftEnd = {
    provider = function()
      return ""
    end,
    condition = should_activate_lsp,
    highlight = { colors.black, colors.divider },
  },
})

insert_left({
  MiddleSpace = {
    provider = function()
      return " "
    end,
    highlight = { colors.black, colors.divider },
  },
})

insert_right({
  RightStart = {
    provider = function()
      return " "
    end,
    highlight = { colors.black, colors.divider },
  },
})

insert_right({
  GitIcon = {
    provider = function()
      return "  "
    end,
    condition = require("galaxyline.provider_vcs").check_git_workspace,
    highlight = { colors.fg, colors.black },
  },
})

insert_right({
  GitBranch = {
    provider = "GitBranch",
    condition = require("galaxyline.provider_vcs").check_git_workspace,
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

insert_right({
  DiffAdd = {
    provider = "DiffAdd",
    condition = checkwidth,
    icon = "  ",
    highlight = { colors.green, colors.black },
  },
})

insert_right({
  DiffModified = {
    provider = "DiffModified",
    condition = checkwidth,
    icon = "  ",
    highlight = { colors.yellow, colors.black },
  },
})

insert_right({
  DiffRemove = {
    provider = "DiffRemove",
    condition = checkwidth,
    icon = "  ",
    highlight = { colors.red, colors.black },
  },
})

insert_right({
  RightEndingSepara = {
    provider = function()
      return ""
    end,
    highlight = { colors.bg, colors.black },
  },
})

insert_right({
  LineInfo = {
    provider = "LineColumn",
    separator = "  ",
    separator_highlight = { colors.fg, colors.bg },
    highlight = { colors.fg, colors.bg },
  },
})

insert_right({
  RightSpacing = {
    provider = function()
      return "   "
    end,
    highlight = { colors.bg, colors.bg },
  },
})

insert_right({
  PerCent = {
    provider = "LinePercent",
    condition = checkwidth,
    highlight = { colors.fg, colors.bg },
  },
})

insert_right({
  RightSpacing = {
    provider = function()
      return "   "
    end,
    highlight = { colors.bg, colors.bg },
  },
})

insert_right({
  FileFormat = {
    provider = "FileEncode",
    condition = checkwidth,
    highlight = { colors.fg, colors.bg },
  },
})

insert_right({
  RightEnding = {
    provider = function()
      return " "
    end,
    highlight = { colors.bg, colors.bg },
  },
})

-- ============================= short line ===============================

local BufferTypeMap = {
  ["DiffviewFiles"] = " Diff View",
  ["FTerm"] = "Terminal",
  ["Mundo"] = "Mundo History",
  ["MundoDiff"] = "Mundo Diff",
  ["NeogitCommitMessage"] = " Neogit Commit",
  ["NeogitPopup"] = " Neogit Popup",
  ["NeogitStatus"] = " Neogit Status",
  ["NvimTree"] = " Tree",
  ["Outline"] = " SymbolOutline",
  ["dap-repl"] = " Dap REPL",
  ["dapui_breakpoints"] = " Dap Breakpoints",
  ["dapui_scopes"] = "כֿ Dap Scope",
  ["dapui_stacks"] = " Dap Stacks",
  ["dapui_watches"] = "ﭓ Dap Watch",
  ["fern"] = " Fern FM",
  ["fugitive"] = " Fugitive",
  ["floggraph"] = " Git Log",
  ["fugitiveblame"] = " Fugitive Blame",
  ["git"] = " Git",
  ["help"] = " Help",
  ["minimap"] = "Minimap",
  ["neoterm"] = " NeoTerm",
  ["qf"] = " Quick Fix",
  ["tabman"] = "Tab Manager",
  ["tagbar"] = "Tagbar",
  ["toggleterm"] = " ToggleTerm",
  ["Trouble"] = "ﮒ Diagnostic",
}

gl.short_line_list = vim.tbl_keys(BufferTypeMap)

require("galaxyline").section.short_line_left = {
  {
    ShortLineLeftBufferType = {
      highlight = { colors.black, colors.cyan },
      provider = function()
        -- return filename for normal file
        local get_file_name = function()
          return string.format("%s %s", "", vim.fn.pathshorten(vim.fn.expand("%")))
        end
        local name = BufferTypeMap[vim.bo.filetype] or get_file_name()
        return string.format("  %s", name)
      end,
      separator = " ",
      separator_highlight = { colors.cyan, colors.bg },
    },
  },
  {
    ShortLineLeftWindowNumber = {
      highlight = { colors.cyan, colors.bg },
      provider = function()
        return " " .. vim.api.nvim_win_get_number(vim.api.nvim_get_current_win())
      end,
      separator = "",
      separator_highlight = { colors.black, colors.bg },
    },
  },
}

require("galaxyline").load_galaxyline()
