if require("libs.cache")["haskell"] then
  return
end

require("lsp").start("hls", {})
