local M = {}

local lshift = require("bit").lshift
local rshift = require("bit").rshift
local band = require("bit").band
local bor = require("bit").bor

-- stylua: ignore
local base64 = {
  [0] = 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
  'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
  'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
  'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
  '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '+', '/',
}
local mask = 0x3f -- 0b00111111

local function encode(s)
  local len = string.len(s)
  local output = {}

  for i = 1, len, 3 do
    local byte1, byte2, byte3 = string.byte(s, i, i + 2)
    local bits = bor(lshift(byte1, 16), lshift(byte2 or 0, 8), byte3 or 0)
    table.insert(output, base64[rshift(bits, 18)])
    table.insert(output, base64[band(rshift(bits, 12), mask)])
    table.insert(output, base64[band(rshift(bits, 6), mask)])
    table.insert(output, base64[band(bits, mask)])
  end

  for i = 0, 1 - ((len - 1) % 3) do
    output[#output - i] = "="
  end

  return table.concat(output)
end

function M.copy(lines)
  local s = table.concat(lines, "\n")
  io.stdout:write(string.format("\x1b]52;;%s\x1b\\", encode(s)))
end

function M.paste()
  local contents = nil
  local id = vim.api.nvim_create_autocmd("TermResponse", {
    callback = function(args)
      local resp = args.data ---@type string
      local encoded = resp:match("\x1b%]52;%w?;([A-Za-z0-9+/=]*)")
      if encoded then
        contents = vim.base64.decode(encoded)
        return true
      end
    end,
  })

  io.stdout:write("\x1b]52;;?\x1b\\")

  vim.wait(1000, function()
    return contents ~= nil
  end)

  -- Delete the autocommand if it didn't already delete itself
  pcall(vim.api.nvim_del_autocmd, id)

  if contents then
    return vim.split(contents, "\n")
  end

  vim.notify("Timed out waiting for a clipboard response from the terminal", vim.log.levels.WARN)
  return 0
end

return M
