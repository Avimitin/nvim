local M = {}

-- errorL notify a message in error level
-- @msg: logging message
-- @title: the logging title
M.errorL = function(msg, title)
  vim.notify(msg, vim.log.levels.ERROR, {
    title = title,
  })
end

-- infoL notify a message in info level
-- @msg: logging message
-- @title: the logging title
M.infoL = function(msg, title)
  vim.notify(msg, vim.log.levels.INFO, {
    title = title,
  })
end

M.fek = function(key, mode)
  vim.fn.feedkeys(vim.api.nvim_replace_termcodes(key, true, true, true), mode)
end

M.dump_tb = function(o)
  if type(o) == "table" then
    local s = "{ "
    for k, v in pairs(o) do
      if type(k) ~= "number" then
        k = '"' .. k .. '"'
      end
      s = s .. "[" .. k .. "] = " .. M.dump_tb(v) .. ","
    end
    return s .. "} "
  else
    return tostring(o)
  end
end

return M
