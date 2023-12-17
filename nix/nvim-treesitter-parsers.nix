{ lib, runCommand, mkTreesitter }:

parsersMetadata:

with builtins;
assert lib.assertMsg (typeOf parsersMetadata == "list" && length parsersMetadata > 0) "Expect `wantedParsers` is a non-empty list";

let
  tsParsers = map mkTreesitter parsersMetadata;
in

runCommand "generate-treesitter-rtp"
{
  inherit tsParsers;
  passthru = { loaderScript = "/treesitter-parsers.lua"; };
} ''
  mkdir -p $out/parser

  parserArray=($tsParsers)
  for drv in ''${parserArray[@]}; do
    # Copy will be way more faster
    cp "$drv"/parser/*.so $out/parser/
  done

  cat <<EOF | tee $out/treesitter-parsers.lua
  vim.opt.rtp:prepend("$out")
  EOF
''
