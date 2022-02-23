require("rest-nvim").setup({
  -- Open request results in a horizontal split
  result_split_horizontal = false,
  -- Skip SSL verification, useful for unknown certificates
  skip_ssl_verification = false,
  -- Highlight request on run
  highlight = {
    enabled = true,
    timeout = 150,
  },
  result = {
    -- toggle showing URL, HTTP info, headers at top the of result window
    show_url = true,
    show_http_info = true,
    show_headers = true,
  },
  -- Jump to request line on run
  jump_to_request = false,
  env_file = ".env",
  custom_dynamic_variables = {},
  yank_dry_run = true,
})

local bufmap = vim.api.nvim_buf_set_keymap
local opts = {
  noremap = true,
  expr = false,
}
bufmap(0, "n", "<Leader>rn", ":lua require('rest-nvim').run()<CR>", opts)
bufmap(0, "n", "<Leader>rp", ":lua require('rest-nvim').run(true)<CR>", opts)
bufmap(0, "n", "<Leader>rl", ":lua require('rest-nvim').last()<CR>", opts)
