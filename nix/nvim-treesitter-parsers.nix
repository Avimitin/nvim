{
  symlinkJoin,
  writeText,
  mkTreesitter,
}:
parsersMetadata:

symlinkJoin rec {
  name = "treesitter-parsers";
  paths = map mkTreesitter parsersMetadata;
  passthru = {
    scriptPath = "/treesitter-parsers.lua";
  };
  postBuild = ''
    echo "vim.opt.rtp:prepend(\"$out\")" > $out/${passthru.scriptPath}
  '';
}
