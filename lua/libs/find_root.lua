local export = {}

function export.get_root(patterns)
  local is_match = function(name, path)
    return vim.tbl_contains(patterns, name)
  end

  local matches = vim.fs.find(is_match, {
    limit = 20,
    upward = true,
    stop = vim.uv.os_homedir(),
    path = vim.fs.dirname(vim.api.nvim_buf_get_name(0)),
  })

  if #matches == 0 then
    return nil
  end

  return matches[1]
end

function export.goto_root(patterns)
  local root = vim.b[0]["root_dir"]
  if root == nil then
    root = export.get_root(patterns)
  else
    vim.api.nvim_set_current_dir(root)
    return true
  end
  vim.b[0]["root_dir"] = root
end

return export
