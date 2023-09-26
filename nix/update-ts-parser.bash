set -e
set -o pipefail

NIX_FILE="./nix/treesitter-parsers.nix"
NIX_EXPR_PREFIX="with import <nixpkgs> {}; with builtins; let parsers = callPackage $NIX_FILE {}; in"

nix_eval() {
  local expr="$@"
  nix eval --impure --raw --show-trace --expr "$NIX_EXPR_PREFIX $expr"
}

nix_hash() {
  local filepath=$1; shift
  nix hash file --base16 --type sha256 --sri $filepath
}

nix_fetch() {
  local url=$1; shift
  nix-prefetch-url $url --print-path --type sha256 | tail -n1
}

get_langs() {
  nix_eval 'lib.pipe (parsers []) [ (map (x: x.pname)) (map (x: lib.last (lib.splitString "-" x))) (concatStringsSep " ") ]'
}

get_src_url() {
  local lang=$1; shift
  nix_eval "parsers.$lang.src.url"
}

get_output_hash() {
  local lang=$1; shift
  nix_eval "parsers.$lang.src.outputHash"
}

main() {
  local langArray=( $(get_langs) )
  for lang in ${langArray[@]}; do
    local url=$(get_src_url $lang)
    local file=$(nix_fetch $url)
    local new_hash=$(nix_hash $file)
    local old_hash=$(get_output_hash $lang)
    [[ $old_hash != $new_hash ]] && \
      echo "Hash for $lang is changed from $old_hash to $new_hash" && \
      sed -i "s|$old_hash|$new_hash|" $NIX_FILE
  done
}

main
