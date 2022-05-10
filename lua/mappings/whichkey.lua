local ok, wk = pcall(require, "which-key")
if not ok then
  require("core.utils").errorL("which key fail to load: " .. wk, "which-key")
  return
end

local keys = {
  gi = {
    name = "+Git",
    B = "Toggle git blame (Popup Panel)",
    b = "Toggle inline git blame (Virtual text)",
    c = "Commit staged changes",
    D = "Toggle diff panel",
    d = "Toggle diff panel",
    h = "Toggle deleted",
    j = "Jump to next hunk",
    k = "Jump to previouse hunk",
    P = "Create a git push prompt",
    p = "Preview current diff",
    r = "Reset current hunk",
    R = "Reset whole buffer",
    S = "Stage current buffer",
    s = "Stage current hunk",
    u = "Undo staged hunk",
  },
  [";"] = {
    name = "+Quick Operation",
    d = "Open cmdline to dispatch commands",
    f = "Open quick find file panel",
    g = "Open git status panel",
    h = "Jump to left window",
    j = "Jump to below window",
    k = "Jump to upper window",
    l = "Jump to right window",
    p = "Enter buffer pick mode",
    q = "Quit current buffer",
    s = "Open live grep panel",
    t = "Open tree file manager",
    w = "Save buffer to file",
  },
  gc = {
    name = "+Comments",
    c = { "Comment current line" },
  },
  ["<leader>"] = {
    t = "Open telescope",
    l = "Open lazygit",
  },
  ["<C-t>"] = {
    l = "Next tab",
    h = "Previous tab",
    n = "New tab",
  },
}

wk.register(keys)

local text_objects = {
  ["af"] = "Select the whole function definition (Include function keyword)",
  ["if"] = "Select the function parameter definition",
  ["ac"] = "Select the whole Class/Struct definition (Include `class/struct` keyword)",
  ["ic"] = "Select the Class fields",
  ["ab"] = "Select the whole block",
  ["ib"] = "Select the block inside",
  ["al"] = "Select the whole function call",
  ["il"] = "Select the function call inside",
}

wk.register(text_objects, { mode = "o", prefix = ""})
