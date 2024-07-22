local export = {}

function export.find_root(opts)
  local current = vim.api.nvim_buf_get_name(0)

  local is_match = function(dir, pattern)
    return vim.fn.globpath(dir, pattern) ~= ""
  end

  local last = vim.uv.os_homedir()
  for dir in vim.fs.parents(current) do
    if dir == vim.uv.os_homedir() then
      return last
    end

    for _, pat in
      ipairs(opts.patterns or { ".git", "go.mod", "flake.nix", "Cargo.toml", "build.sc" })
    do
      if is_match(dir, pat) then
        return dir
      end
    end

    last = dir
  end

  return last
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
