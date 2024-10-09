local export = {}

function export.find_root(opts)
  return vim.fs.root(0, opts.patterns or { ".git", "flake.nix" })
    or vim.fs.parents(vim.api.nvim_buf_get_name(0))
end

function export.set_root(opts)
  opts = opts or {}

  if not vim.tbl_contains({ "", "acwrite" }, vim.bo.buftype) then
    return
  end

  if vim.tbl_contains(opts.exclude_filetype or {}, vim.bo.filetype) then
    return
  end

  local root = vim.b.current_buf_root_dir or nil
  if root == nil then
    root = export.find_root(opts)
    vim.b.current_buf_root_dir = root
  end

  if root ~= nil then
    vim.api.nvim_set_current_dir(root)
  end
end

return export
