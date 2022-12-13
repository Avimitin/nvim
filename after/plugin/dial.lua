if require("libs.g").dial then
  return
end

local nmap = require("libs.keymaps").nmap
local map = require("libs.keymaps").map
local function _cmd_sequence(direction, mode)
  local function cmd(body)
    local cmd_sequences = "<Cmd>"
    local cr_sequences = "<CR>"
    return cmd_sequences .. body .. cr_sequences
  end

  local function if_expr(cond, branch_true, branch_false)
    if cond then
      return branch_true
    end
    return branch_false
  end

  local select = cmd([[lua require"dial.command".select_augend_]] .. mode .. "()")
  local setopfunc = cmd([[let &opfunc="dial#operator#]] .. direction .. "_" .. mode .. [["]])
  local textobj = if_expr(mode == "normal", cmd([[lua require("dial.command").textobj()]]), "")
  return select .. setopfunc .. "g@" .. textobj
end

nmap("=", _cmd_sequence("increment", "normal"))
nmap("-", _cmd_sequence("decrement", "normal"))
map("v", "=", _cmd_sequence("increment", "visual"))
map("v", "-", _cmd_sequence("decrement", "visual"))
