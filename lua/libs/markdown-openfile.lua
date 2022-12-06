local fn = vim.fn

local _config = {}

-- @field ln: line number
-- @field col: column number
local function corner_of_syntax(step, ln, col)
  local current_syntax = fn.synIDattr(fn.synID(ln, col, 1), "name")
  while fn.synIDattr(fn.synID(ln, col, 1), "name") == current_syntax do
    col = col + step
  end

  return col - step
end

local function find_next_syntax(ln, col, name)
  local step = 1
  while fn.synIDattr(fn.synID(ln, col, 1), "name") ~= name do
    col = col + step
  end

  return { ln, col }
end

local function find_right_of_syntax(...)
  return corner_of_syntax(1, ...)
end

local function find_left_of_syntax(...)
  return corner_of_syntax(-1, ...)
end

local function find_corners_of_syntax(...)
  return {
    find_left_of_syntax(...),
    find_right_of_syntax(...),
  }
end

local function get_url_from_position(ln, col)
  local syntax = fn.synIDattr(fn.synID(ln, col, 1), "name")

  if syntax == "markdownLink" then
    local next = find_next_syntax(ln, col, "markdownUrl")
    ln, col = next[1], next[2]
    syntax = "markdownUrl"
  elseif syntax == "markdownDelimiter" then
    local line = fn.getline(ln)
    local char = line[col]
    if char == "<" then
      col = col + 1
    elseif char == ">" or char == ")" then
      col = col - 1
    elseif char == "[" or char == "]" or char == "(" then
      local next = find_next_syntax(ln, col, "markdownUrl")
      ln, col = next[1], next[2]
    else
      return ""
    end
  elseif syntax ~= "mkdInlineUrl" and syntax ~= "markdownUrl" and syntax ~= "mkdLinkDefTarget" then
    return ""
  end

  local corners = find_corners_of_syntax(ln, col)
  local left, right = corners[1], corners[2]
  local line = fn.getline(ln)
  return string.sub(line, left, right)
end

local function edit_url_under_cursor()
  local edit_method = "edit"
  if _config.edit_method ~= nil then
    edit_method = _config.edit_method
  end

  local url = get_url_from_position(fn.line("."), fn.col("."))
  if url == "" then
    fn.execute(edit_method .. " <cfile>")
    return
  end

  local anchor = ""
  local parts = {}
  for match in string.gmatch(url, "([^#]+)") do
    table.insert(parts, match)
  end

  if #parts == 2 then
    url = parts[1]
    anchor = parts[2]
  end

  if url == "" then
    if anchor ~= "" then
      fn.execute("/" .. anchor)
    end
    return
  end

  local ext = ""
  if _config.no_extention_for_file_link then
    if _config.custom_ext then
      ext = "." .. _config.custom_ext
    else
      ext = ".md"
    end
  end
  url = fn.fnameescape(fn.fnamemodify(fn.expand("%:h") .. "/" .. url .. ext, ":."))
  fn.execute(edit_method .. " " .. url)
end

local M = {}

M.setup = function(opts)
  _config = vim.tbl_deep_extend("force", _config, opts)
end

M.edit_url = edit_url_under_cursor

return M
