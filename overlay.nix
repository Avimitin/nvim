final: prev:
{
  mkTreesitter = final.callPackage ./nix/mkTreesitter.nix { };
  fetchNvimTsLockFile = final.callPackage ./nix/nvim-treesitter-lock-file.nix { };
  generate-nvim-treesitter-parsers = final.callPackage ./nix/nvim-treesitter-parsers.nix { };

  neovim-nightly-bin =
    let
      srcInfo = final.callPackage ./nix/_sources/generated.nix { };
    in
    final.neovim-unwrapped.overrideAttrs {
      inherit (srcInfo.neovim-nightly) version src;
    };
  neovim = final.wrapNeovim final.neovim-nightly-bin { };
}
