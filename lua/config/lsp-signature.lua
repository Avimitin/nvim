-- Copy from: https://github.com/siduck76/NvChad/commit/d4193727bd474d4454fa8fdec6764bf2064aaf5b
local present, lspsignature = pcall(require, "lsp_signature")
if present then
    lspsignature.setup({
        bind = true,
        doc_lines = 2,
        floating_window = true,
        fix_pos = true,
        hint_enable = true,
        hint_prefix = " ",
        hint_scheme = "String",
        hi_parameter = "Search",
        max_height = 22,
        max_width = 120, -- max_width of signature floating_window, line will be wrapped if exceed max_width
        handler_opts = {
            border = "single" -- double, single, shadow, none
        },
        zindex = 50, -- by default it will be on top of all floating windows, set to 50 send it to bottom
        padding = "" -- character to pad on left and right of signature can be ' ', or '|'  etc
    })
end
