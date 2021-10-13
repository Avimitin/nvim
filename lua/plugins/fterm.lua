if not pcall(require, 'FTerm') then
    print("Failed to call FTerm")
    return
end

require('FTerm').setup {
    dimensions = {height = 0.9, width = 0.9},

    border = 'rounded',

    hl = 'NormalFloat'
}

vim.cmd('command! FTermToggle lua require("FTerm").toggle()')
