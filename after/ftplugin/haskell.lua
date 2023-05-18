if require("libs.cache")["haskell"] then
  return
end

require("lang").run_lsp("hls", {})
