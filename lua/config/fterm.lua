if not pcall(require, 'FTerm') then
    vim.notify("Fail to load FTerm", vim.log.levels.ERROR, {title='plugins'})
    return
end

require('FTerm').setup {
    dimensions = {height = 0.9, width = 0.9},

    border = 'shadow',

    hl = 'FTermBackground'
}

vim.cmd('command! FTermToggle lua require("FTerm").toggle()')
