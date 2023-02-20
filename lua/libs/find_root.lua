local export = {}

local function match(dir, pattern)
  if string.sub(pattern, 1, 1) == "=" then
    return vim.fn.fnamemodify(dir, ":t") == string.sub(pattern, 2, #pattern)
  else
    return vim.fn.globpath(dir, pattern) ~= ""
  end
end

function export.get_root(root_pattern)
  local current = vim.api.nvim_buf_get_name(0)
  local find_parent = function(dir)
    return vim.fn.fnamemodify(dir, ":h")
  end
  local parent_dir = find_parent(current)

  while 1 do
    for _, pattern in ipairs(root_pattern or { ".git", ".hg", ".svn" }) do
      if match(parent_dir, pattern) then
        return parent_dir
      end
    end

    current, parent_dir = parent_dir, find_parent(parent_dir)
    if parent_dir == current then
      break
    end
  end
  return nil
end

function export.find_root(root_pattern)
  local root = vim.fn.exists("b:root_dir") == 1 and vim.api.nvim_buf_get_var(0, "root_dir") or nil
  if root == nil then
    root = export.get_root(root_pattern)
    vim.api.nvim_buf_set_var(0, "root_dir", root)
  end

  if root ~= nil then
    vim.api.nvim_set_current_dir(root)
  end
end

return export
