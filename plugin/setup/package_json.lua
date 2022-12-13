if require("libs.g")["package_json"] then
  return
end

vim.api.nvim_create_autocmd("BufRead", {
  pattern = "package.json",
  callback = function(props)
    require("packer").loader("package-info.nvim")
    require("overlays.rc.js_deps").setup_hydra(props.buf)
  end,
})
