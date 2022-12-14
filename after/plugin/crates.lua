if require("libs.g").crates then
  return
end

vim.api.nvim_create_autocmd("BufRead", {
  pattern = "Cargo.toml",
  callback = function(props)
    require("packer").loader("crates.nvim")
    require("overlays.rc.crates").setup_hydra(props.buf)
    require("crates").update(props.buf)
  end,
})
