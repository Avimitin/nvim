{ fetchFromGitHub, callPackage }:
let
  mkTreesitter = callPackage ./treesitter.nix {};
in
mkTreesitter {
  lang = "nix";
  src = fetchFromGitHub {
    owner = "nix-community";
    repo = "tree-sitter-nix";
    rev = "66e3e9ce9180ae08fc57372061006ef83f0abde7";
    sha256 = "sha256-+o+f1TlhcrcCB3TNw1RyCjVZ+37e11nL+GWBPo0Mxxg=";
  };
  version = "0.0.0+rev=66e3e9c";
}
