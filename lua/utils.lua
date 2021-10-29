local M = {}

M.map = function (mode, lhs, rhs, opts)
    local options = {noremap = true, silent = true}
    if opts then
      options = vim.tbl_extend("force", options, opts)
    end
    local stat, error = pcall(vim.api.nvim_set_keymap, mode, lhs, rhs, options)
    if not stat then
      vim.notify(error, vim.log.levels.ERROR, {title='keymap'})
    end
end

M.new_cmd = function (cmd, repl)
    local ok, err = pcall(vim.cmd, "command "..cmd.." "..repl)
    if not ok then
      vim.notify("setting cmd: "..cmd.." "..err, vim.log.levels.ERROR, {title='command'})
    end
end

return M
