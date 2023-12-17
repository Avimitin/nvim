final: prev:
{
  mkTreesitter = final.callPackage ./nix/mkTreesitter.nix { };
  nvim-treesitter-lock-file = final.callPackage ./nix/nvim-treesitter-lock-file.nix { };
  generate-nvim-treesitter-parsers = final.callPackage ./nix/nvim-treesitter-parsers.nix { };
}
