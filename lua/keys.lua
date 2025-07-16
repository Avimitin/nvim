-- This file act as an API compatible layer.
-- Here we are using which-key.nvim as plugin, but when one day which-key.nvim
-- is no more under maintain, we can still keep compatibiltity and replace implementation here.

local keymapper = {}

local function unwrap_options(original)
  local default = {
    noremap = true,
    silent = true,
    desc = "Undocumented",
  }

  return vim.tbl_deep_extend("force", default, original)
end

local function unwrap_keymaps(mappings)
  local copy = vim.tbl_deep_extend("force", {}, mappings)
  copy[1] = nil
  copy[2] = nil

  return mappings[1], mappings[2], unwrap_options(copy)
end

local function check(mappings)
  local warn = function(msg)
    vim.notify(msg, vim.log.levels.WARN)
  end
  if not mappings or #mappings == 0 then
    local is_buffer_map = mappings.buffer ~= nil and "true" or "false"
    warn("Null mappings given, please check config: is_buffer_map=" .. is_buffer_map)
    return false
  end
  if #mappings < 2 then
    warn("Only one key mapping is given: " .. mappings[1])
    return false
  end
  if #mappings > 2 then
    local msg = string.format(
      "More than two pair was given, where first item is %s, second item is %s",
      mappings[1],
      mappings[2]
    )
    warn(msg)
    return false
  end
  if type(mappings[1]) ~= "string" then
    warn("Mapping pairs must be in string, but lhs is not, it is a " .. type(mappings[1]))
    return false
  end
  if type(mappings[2]) ~= "string" and type(mappings[2]) ~= "function" then
    warn(
      "Mapping pairs must be in string or function, but rhs is not, it is a " .. type(mappings[2])
    )
    return false
  end

  return true
end

-- Batch set keymappings
-- Allow form:
--   1. { "lhs", "rhs", option = "foo" }
--   2. { { "lhs", "rhs", opt = "foo" }, { "lhs", "rhs", opt = "bar" } }
function keymapper.map(modes, mappings)
  if type(modes) == "string" then
    modes = { modes }
  end

  if type(modes) ~= "table" then
    vim.notify("Invalid mode set when mapping keys", vim.log.levels.ERROR)
    return false
  end

  for _, mode in ipairs(modes) do
    if type(mappings[1]) == "string" then
      if not check(mappings) then
        return false
      end

      vim.keymap.set(mode, unwrap_keymaps(mappings))
      goto continue
    end

    for _, kpair in ipairs(mappings) do
      if not check(kpair) then
        return false
      end
      vim.keymap.set(mode, unwrap_keymaps(kpair))
    end

    ::continue::
  end
end

function keymapper.cmd(cmd)
  return "<CMD>" .. cmd .. "<CR>"
end

function keymapper.bufmap(bufid, modes, mappings)
  if type(mappings[1]) == "string" then
    mappings["buffer"] = bufid
    return keymapper.map(modes, mappings)
  end

  for _, kpair in ipairs(mappings) do
    kpair["buffer"] = bufid
  end
  return keymapper.map(modes, mappings)
end

function keymapper.mk_keymap(keymap_set)
  keymapper.map("n", keymap_set.normal)
  keymapper.map("x", keymap_set.selection)
  keymapper.map("i", keymap_set.insertion)
  keymapper.map("t", keymap_set.terminal)
end

return keymapper
