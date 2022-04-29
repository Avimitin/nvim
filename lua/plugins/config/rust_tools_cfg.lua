local ok, error = pcall(require, "rust-tools")
if not ok then
  vim.notify(error)
  return
end

local function parse_config(path)
  local json = require("cjson")
  local loaded, file = pcall(io.open, path, "rb")
  if not loaded or not file then
    -- We shouldn't notify when file is not exist, let it be silent
    -- vim.notify("fail to read rust-analyer settings from file " .. path .. ": " .. file)
    return nil
  end
  local content = file:read("*all")
  file:close()
  local parse_ok, setting = pcall(json.decode, content)
  if not parse_ok then
    vim.notify("fail to parse rust-analyer settings from file " .. path .. ": " .. setting)
    return nil
  end
  return setting
end

local function find_ra_settings()
  -- cd to the directory which contains "Cargo.toml" file
  require("packer").loader("vim-rooter")
  vim.cmd("Rooter")
  local filename = ".rust-analyzer.json"
  local cwd = vim.fn.getcwd()
  if cwd:sub(-1) == "/" then
    return cwd .. filename
  else
    return cwd .. "/" .. filename
  end
end

local filename = find_ra_settings()
local settings = parse_config(filename)
local default = {
  cargo = {
    autoreload = true,
  },
}

if settings then
  default = vim.tbl_deep_extend("force", default, settings)
end

local opts = {
  tools = {
    autoSetHints = true,
    hover_with_actions = true,
    executor = require("rust-tools/executors").termopen,

    -- These apply to the default RustSetInlayHints command
    inlay_hints = {
      show_parameter_hints = true,
      show_variable_name = true,
      parameter_hints_prefix = "<- ",
      other_hints_prefix = "=> ",
      max_len_align = false,
      max_len_align_padding = 1,
      right_align = false,
      right_align_padding = 7,
    },

    hover_actions = {
      border = {
        { "╭", "FloatBorder" },
        { "─", "FloatBorder" },
        { "╮", "FloatBorder" },
        { "│", "FloatBorder" },
        { "╯", "FloatBorder" },
        { "─", "FloatBorder" },
        { "╰", "FloatBorder" },
        { "│", "FloatBorder" },
      },
      auto_focus = true,
    },
  },
  server = {
    settings = {
      ["rust-analyzer"] = default,
    },
    on_attach = require("plugins.config.lspconfig_cfg").set_lsp_key,
  }, -- rust-analyer options
}

require("rust-tools").setup(opts)

vim.g.rustfmt_options = "--edition=2021"
