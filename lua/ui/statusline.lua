local gl = require("galaxyline")
local gls = gl.section

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

local checkwidth = function()
  local squeeze_width = vim.fn.winwidth(0) / 2
  if squeeze_width > 50 then
    return true
  end
  return false
end

local function should_activate_lsp()
  local clients = vim.lsp.get_active_clients({ bufnr = 0 })
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

-----------------------------------------------------
----------------- start insert ----------------------
-----------------------------------------------------
-- { mode panel start
insert_left({
  LeftSpace = {
    provider = function()
      return "  "
    end,
    highlight = { colors.bg, colors.bg },
  },
})

local vim_mode = {
  alias = {
    n = " Normal",
    no = "󰌌 Pending",
    nov = "󰌌 Pending",
    noV = "󰌌 Pending",
    i = " Insert",
    c = " Command",
    v = " Visual",
    V = " Visual",
    [""] = "󰩬 Region",
    C = "",
    ["r?"] = "?",
    rm = "M",
    R = " Replace",
    Rv = " Virtual Replace",
    s = "",
    S = "",
    ["r"] = "HIT-ENTER",
    [""] = "",
    t = " Terminal",
    ["!"] = " Shell",
    _LineLeap = "󱕘 LineLeap",
  },
  color = {
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
    _LineLeap = colors.green,
  },
  is_line_leap = false,
}

insert_left({
  ViModeIcon = {
    provider = function()
      -- auto change color according the vim mode
      local mode = vim.fn.mode()
      if vim_mode.is_line_leap then
        mode = "_LineLeap"
      end
      vim.api.nvim_set_hl(0, "GalaxyViMode", { fg = vim_mode.color[mode], bg = colors.bg })
      return vim_mode.alias[mode]
    end,
    highlight = "GalaxyViMode",
  },
})

local gid = vim.api.nvim_create_augroup("LineLeapStatus", { clear = true })
vim.api.nvim_create_autocmd("User", {
  pattern = "LeapEnter",
  group = gid,
  callback = function()
    vim_mode.is_line_leap = true
  end,
})
vim.api.nvim_create_autocmd("User", {
  pattern = "LeapLeave",
  group = gid,
  callback = function()
    vim_mode.is_line_leap = false
  end,
})

insert_left({
  LeftBigSpace = {
    provider = function()
      return "  | "
    end,
    highlight = { colors.divider, colors.bg },
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
      colors.bg,
    },
  },
})

insert_left({
  GetLspClient = {
    provider = "GetLspClient",
    condition = should_activate_lsp,
    highlight = { colors.fg, colors.bg },
  },
})

insert_left({
  LspSpace = {
    provider = function()
      return "  "
    end,
    condition = should_activate_lsp,
    highlight = { colors.bg, colors.bg },
  },
})

insert_left({
  DiagnosticError = {
    provider = "DiagnosticError",
    condition = should_activate_lsp,
    icon = "  ",
    highlight = { colors.red, colors.bg },
  },
})

insert_left({
  DiagnosticWarn = {
    provider = "DiagnosticWarn",
    condition = should_activate_lsp,
    icon = "  ",
    highlight = { colors.yellow, colors.bg },
  },
})

insert_left({
  DiagnosticInfo = {
    provider = "DiagnosticInfo",
    condition = should_activate_lsp,
    highlight = { colors.green, colors.bg },
    icon = "  ",
  },
})

insert_left({
  DiagnosticHint = {
    provider = "DiagnosticHint",
    condition = should_activate_lsp,
    highlight = { colors.white, colors.bg },
    icon = "  ",
  },
})

insert_left({
  MiddleSpace = {
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
  ["neo-tree"] = " Files",
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
      highlight = { colors.dimblue, colors.bg },
      provider = function()
        -- return filename for normal file
        local get_file_name = function()
          return string.format("%s %s", "", vim.fs.basename(vim.fn.expand("%")))
        end
        local name = BufferTypeMap[vim.bo.filetype] or get_file_name()
        return string.format("  %s", name)
      end,
      separator = " ",
      separator_highlight = { colors.bg, colors.bg },
    },
  },
  {
    WinSeparator = {
      highlight = { colors.black, colors.black },
      provider = function()
        return " "
      end,
    },
  },
}

require("galaxyline").load_galaxyline()
