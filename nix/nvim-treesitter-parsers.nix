{
  symlinkJoin,
  writeText,
  mkTreesitter,
}:
parsersMetadata:

let
  plugins = map mkTreesitter parsersMetadata;
in
symlinkJoin rec {
  name = "treesitter-parsers";
  paths = plugins;
  passthru = {
    inherit plugins;
    scriptPath = "/treesitter-parsers.lua";
  };
  postBuild = ''
    echo "vim.opt.rtp:prepend(\"$out\")" > $out/${passthru.scriptPath}
  '';
}
