-- Library for batch key mappings
--
-- Usage:
--  require("libs.keymap").nmap({ "lhs", "rhs", noremap = true })
--  require("libs.keymap").xmap({ { "lhs", "rhs" }, { "lhs2", "rhs2", desc = "foo bar" } })
--

local M = {}

local function unwrap_options(original)
  local default = {
    noremap = true,
    silent = true,
    desc = "Undocumented",
  }

  return vim.tbl_deep_extend("force", default, original)
end

local function unwrap_keymaps(mappings)
  local lhs = mappings[1]
  local rhs = mappings[2]
  mappings[1] = nil
  mappings[2] = nil

  return lhs, rhs, unwrap_options(mappings)
end

function M.map(mode, mappings)
  if mappings == nil then
    return
  end

  if #mappings == 0 then
    return
  end

  if type(mappings[1]) == "string" then
    vim.keymap.set(mode, unwrap_keymaps(mappings))
  end

  for _, kpair in ipairs(mappings) do
    vim.keymap.set(mode, unwrap_keymaps(kpair))
  end
end

function M.nmap(mappings)
  M.map("n", mappings)
end

function M.xmap(mappings)
  M.map("x", mappings)
end

function M.imap(mappings)
  M.map("i", mappings)
end

function M.tmap(mappings)
  M.map("t", mappings)
end

function M.wrap_cmd(command)
  return "<CMD>" .. command .. "<CR>"
end

function M.buf_map(buffer_id, mode, mappings)
  if mappings == nil then
    return
  end

  if #mappings == 0 then
    return
  end

  if type(mappings[1]) == "string" then
    mappings["buffer"] = buffer_id
    vim.keymap.set(mode, unwrap_keymaps(mappings))
  end

  for _, kpair in ipairs(mappings) do
    kpair["buffer"] = buffer_id
    vim.keymap.set(mode, unwrap_keymaps(kpair))
  end
end

return M
