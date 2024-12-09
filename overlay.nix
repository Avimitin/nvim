final: prev:
{
  mkTreesitter = final.callPackage ./nix/mkTreesitter.nix { };
  fetchNvimTsLockFile = final.callPackage ./nix/nvim-treesitter-lock-file.nix { };
  generate-nvim-treesitter-parsers = final.callPackage ./nix/nvim-treesitter-parsers.nix { };

  treesitter-plugin-nightly = final.generate-nvim-treesitter-parsers [
    { name = "bash"; hash = "sha256-MwNk0TdqMqcUZFdONZim2yUZNblcaZSF9Rg1+LRCofM="; }
    { name = "c"; hash = "sha256-4HQmlOz0GVqWH0Ho5IrTgOckHiVGrNP0TB1pe5qbT5o="; }
    { name = "cpp"; hash = "sha256-+YUH//H+skIC2EH8L+OJv5Go6a7l+5/N52oy5o+LWQ8="; }
    { name = "css"; hash = "sha256-RbVqBuxXb7qPuGpT34Cn6FujpnNnAmeDsG8Y1hFf2Ms="; }
    { name = "comment"; hash = "sha256-XSkreGkg8yhTDPBPU2yZ8apXcEbNJ0csjv5xnp9vvwQ="; }
    { name = "diff"; hash = "sha256-r2DgtdvwLWPYGG3Rn8WPAxz2JQP3CMNFRfrvToceZTc="; }
    { name = "firrtl"; hash = "sha256-xrh8XlgcFz+Z1Rh7z1KZWV26RE65hVtIKjw9XPZaXuA="; }
    { name = "gitcommit"; hash = "sha256-DDBJYsJNQggyuW2IsB2zjDPEqen7mKhe7Xj5o0DwUWQ="; }
    { name = "haskell"; hash = "sha256-CW6yMY9bDUq7rB4JjaDK7anWNei5RenovwK+i0+dVZQ="; }
    { name = "javascript"; hash = "sha256-BWUW2wr5cvbNTZCMn3tn1vfOFEASYTLLgKnd848bVRU="; }
    { name = "typescript"; hash = "sha256-Mvt1vLMowpLBwIcqbwBR2vlws4AtkAHbhYwPHFsgEGE="; srcRoot = "typescript"; }
    { name = "tsx"; hash = "sha256-Mvt1vLMowpLBwIcqbwBR2vlws4AtkAHbhYwPHFsgEGE="; srcRoot = "tsx"; }
    { name = "typst"; hash = "sha256-nUrndgD1wEfC3HNpTaj9xfYZi3fE9VoRf+etJc9oz8k="; }
    { name = "llvm"; hash = "sha256-3/WPq64L+L0ewbrbvihkeuLOCoS7XhiKQnFIwS3i4HY="; }
    { name = "lua"; hash = "sha256-bWW1MZQukBCTRGxBHXZY0vQ8MwFrft8GuNDatfPhYDI="; }
    { name = "ocaml"; hash = "sha256-PXEenvdqXgkyXqSKtxXtUEmw02mxyR1pnGkwGXlXC2U="; srcRoot = "grammars/ocaml"; }
    { name = "ocaml_interface"; hash = "sha256-PXEenvdqXgkyXqSKtxXtUEmw02mxyR1pnGkwGXlXC2U="; srcRoot = "grammars/interface"; }
    { name = "regex"; hash = "sha256-3XENXjzQjtdzScKZLk3vtnNfOMrqKYfE9ZYgDxye1gk="; }
    { name = "ruby"; hash = "sha256-5SDUzaKAXbgrWkTshLoEjjDZF3CFG0oXNJDg1F5MtVU="; }
    { name = "python"; hash = "sha256-mdKCVLxACWi23UTiqklfEdCRzWV9h9Qi/7uXAZtPUHs="; }
    { name = "rust"; hash = "sha256-8joGBf/DPZAdj9ClSGD8+oRaEMBJiGaGgUaXq+ukUqM="; }
    { name = "proto"; hash = "sha256-eDnzT35wGxFzhcvy61d+1VG8ObB999mcakG3NNlrcck="; }
    { name = "scala"; hash = "sha256-GgdsthebdMqYUJrUl3CpYuICmoxCHkC+GISRReQ8+s8="; }
    { name = "nix"; hash = "sha256-QWo8bVG/0mqRlCmnx1iL3mqpIP8wE6YYd2Yr0IxTgY8="; }
    { name = "vimdoc"; hash = "sha256-zcUVFK2CCGiHk7jfx2QwXmjOAApBnM0Q9HyMMiDhCHU="; }
    { name = "query"; hash = "sha256-zBhYS12/tLp8lA8UW6kzDnH2nXeCfGrcW2pYynvj+2Y="; }
    {
      name = "markdown";
      hash = "sha256-sLV5sbCTVr1kaQnHdspqvjqP9uB3EgmFLheW3ZMBhnw=";
      srcRoot = "tree-sitter-markdown";
    }
    {
      name = "markdown_inline";
      hash = "sha256-sLV5sbCTVr1kaQnHdspqvjqP9uB3EgmFLheW3ZMBhnw=";
      srcRoot = "tree-sitter-markdown-inline";
    }
    {
      name = "mlir";
      hash = "sha256-hX0p5WUJisdVtM6gRq6B86xYn/klQPnNQp0L3RMtk7E=";
      needs_generate = true;
    }
    { name = "yaml"; hash = "sha256-8z/jqz2XZ9dJgZQigki7TS0UYhSQGpcY84IE4B0HStQ="; }
    { name = "zig"; hash = "sha256-y9fmepjJ0tfk0o4uh/SWViXYZHlb9bRiQb+Dn6rpXyw="; }
  ];

  neovim-nightly-bin = final.neovim-unwrapped.overrideAttrs {
    # Disable default treesitter plugins, they are outdated
    treesitter-parsers = { };
  };
  neovim-nightly = final.wrapNeovim final.neovim-nightly-bin {
    extraMakeWrapperArgs = '' '--add-flags' '--cmd "set rtp^=${final.treesitter-plugin-nightly}"' '';
  };

  ghc-for-ts-plugins = final.haskellPackages.ghcWithPackages (pkgs: with pkgs; [
    aeson
    turtle
  ]);
}
