{ lib, runCommand, mkTreesitter, wantedParsers ? [ ] }:

with builtins;
assert lib.assertMsg (typeOf wantedParsers == "list" && length wantedParsers > 0) "Expect `wantedParsers` is a non-empty list";

let
  tsParsers = map mkTreesitter wantedParsers;
in

runCommand "generate-treesitter-rtp"
{
  inherit tsParsers;
  passthru = { luaScript = "/treesitter-parsers.lua"; };
} ''
  mkdir -p $out/parser

  parserArray=($tsParsers)
  for drv in ''${parserArray[@]}; do
    ln -s "$drv"/parser/*.so $out/parser/
  done

  cat <<EOF | tee $out/treesitter-parsers.lua
  vim.opt.rtp:prepend("$out")
  EOF
''
