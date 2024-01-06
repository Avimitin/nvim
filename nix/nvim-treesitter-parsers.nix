{ lib, runCommand, writeShellApplication, jq, mkTreesitter }:

parsersMetadata:

with builtins;
assert lib.assertMsg (typeOf parsersMetadata == "list" && length parsersMetadata > 0) "Expect `wantedParsers` is a non-empty list";

let
  tsParsers = map mkTreesitter parsersMetadata;
in

runCommand "generate-treesitter-rtp"
{
  inherit tsParsers;
  passthru = {
    luaPath = "/treesitter-parsers.lua";
    updateScript = writeShellApplication {
      name = "treesitter-hash-batch-updater";
      runtimeInputs = [ jq ];
      text = ''
        [ -z "$*"  ] && echo "No nix file specify" && exit 1
        nix_file="$1"

        log() {
          echo ">>> [updater] $*"
        }

        getVal() {
          nix derivation show "$1" | jq -r "to_entries|.[0].value$2"
        }

        declare -a parsersArray
        parsersArray=( ${toString tsParsers} )
        for parser in "''${parsersArray[@]}"; do
          log "Verifing parser $parser"

          src=$(getVal "$parser" ".env.src")
          oldHash=$(getVal "$src" ".env.outputHash")
          srcUrl=$(getVal "$src" ".env.urls")
          newFile=$(nix-prefetch-url "$srcUrl" --print-path --type sha256 | tail -n1)
          newHash=$(nix hash file --base16 --type sha256 --sri "$newFile")

          if [[ "$newHash" != "$oldHash" ]]; then
            log "Hash changed for parser $parser"
            sed -i "s/$oldHash/$newHash/" "$nix_file"
          else
            log "Parser $parser is up to date"
          fi
        done

        log "All hash are updated in $nix_file"
      '';
    };
  };
} ''
  mkdir -p $out/parser
  mkdir -p $out/dist

  parserArray=($tsParsers)
  for drv in ''${parserArray[@]}; do
    # Copy will be way more faster
    cp "$drv"/parser/*.so $out/parser/
    cp "$drv"/dist/*-version.txt $out/dist/
  done

  cat <<EOF | tee $out/treesitter-parsers.lua
  vim.opt.rtp:prepend("$out")
  EOF
''
