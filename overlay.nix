final: prev:
{
  mkTreesitter = final.callPackage ./nix/mkTreesitter.nix { };
  fetchNvimTsLockFile = final.callPackage ./nix/nvim-treesitter-lock-file.nix { };
  generate-nvim-treesitter-parsers = final.callPackage ./nix/nvim-treesitter-parsers.nix { };

  treesitter-plugin-nightly = final.generate-nvim-treesitter-parsers [
    { name = "bash"; hash = "sha256-MwNk0TdqMqcUZFdONZim2yUZNblcaZSF9Rg1+LRCofM="; }
    { name = "c"; hash = "sha256-FBELk9G2j30hds+lUAQyH4g6lA2zY1BhEA7lqbwLO4Q="; }
    { name = "cpp"; hash = "sha256-+YUH//H+skIC2EH8L+OJv5Go6a7l+5/N52oy5o+LWQ8="; }
    { name = "css"; hash = "sha256-RbVqBuxXb7qPuGpT34Cn6FujpnNnAmeDsG8Y1hFf2Ms="; }
    { name = "comment"; hash = "sha256-XSkreGkg8yhTDPBPU2yZ8apXcEbNJ0csjv5xnp9vvwQ="; }
    { name = "diff"; hash = "sha256-Q61DD6CX81M3npBflPE5P3glMdfCNB877juXFgKqu7Q="; }
    { name = "firrtl"; hash = "sha256-xrh8XlgcFz+Z1Rh7z1KZWV26RE65hVtIKjw9XPZaXuA="; }
    { name = "gitcommit"; hash = "sha256-DDBJYsJNQggyuW2IsB2zjDPEqen7mKhe7Xj5o0DwUWQ="; }
    { name = "haskell"; hash = "sha256-CW6yMY9bDUq7rB4JjaDK7anWNei5RenovwK+i0+dVZQ="; }
    { name = "javascript"; hash = "sha256-BWUW2wr5cvbNTZCMn3tn1vfOFEASYTLLgKnd848bVRU="; }
    { name = "typescript"; hash = "sha256-u0gKMzytoSxBwR0pzFV8FTNXi7pepqDkcRs0RyCWLNw="; srcRoot = "typescript"; }
    { name = "tsx"; hash = "sha256-u0gKMzytoSxBwR0pzFV8FTNXi7pepqDkcRs0RyCWLNw="; srcRoot = "tsx"; }
    { name = "typst"; hash = "sha256-nUrndgD1wEfC3HNpTaj9xfYZi3fE9VoRf+etJc9oz8k="; }
    { name = "llvm"; hash = "sha256-3/WPq64L+L0ewbrbvihkeuLOCoS7XhiKQnFIwS3i4HY="; }
    { name = "lua"; hash = "sha256-bWW1MZQukBCTRGxBHXZY0vQ8MwFrft8GuNDatfPhYDI="; }
    { name = "ocaml"; hash = "sha256-OIwLMzR80zy7g/UgfVXh3X/dVtquemHExVfHY7LNKzY="; srcRoot = "grammars/ocaml"; }
    { name = "ocaml_interface"; hash = "sha256-OIwLMzR80zy7g/UgfVXh3X/dVtquemHExVfHY7LNKzY="; srcRoot = "grammars/interface"; }
    { name = "regex"; hash = "sha256-HXzc5qQEhLZbnDUUXsq13S0NzopYg/t7le2YKqpSF54="; }
    { name = "ruby"; hash = "sha256-5SDUzaKAXbgrWkTshLoEjjDZF3CFG0oXNJDg1F5MtVU="; }
    { name = "python"; hash = "sha256-KCRdSrhG3xEBMNIsXJQyePu7TZhQ0i+ru9Fe9gfkFMg="; }
    { name = "rust"; hash = "sha256-snBugAU0Cp7SCxEJmJ77MbUvNpztVeOJZUR6fLrV0JU="; }
    { name = "proto"; hash = "sha256-eDnzT35wGxFzhcvy61d+1VG8ObB999mcakG3NNlrcck="; }
    { name = "scala"; hash = "sha256-sfn34imRxJeev+JWP611X+94lF0YPBA042E8BVCy+IM="; }
    { name = "nix"; hash = "sha256-QWo8bVG/0mqRlCmnx1iL3mqpIP8wE6YYd2Yr0IxTgY8="; }
    { name = "vimdoc"; hash = "sha256-zcUVFK2CCGiHk7jfx2QwXmjOAApBnM0Q9HyMMiDhCHU="; }
    { name = "query"; hash = "sha256-zBhYS12/tLp8lA8UW6kzDnH2nXeCfGrcW2pYynvj+2Y="; }
    {
      name = "markdown";
      hash = "sha256-A05oSxMZ9oMVGW/ampKMNxltgqG2ffPIWDTLbXeFM/o=";
      srcRoot = "tree-sitter-markdown";
    }
    {
      name = "markdown_inline";
      hash = "sha256-A05oSxMZ9oMVGW/ampKMNxltgqG2ffPIWDTLbXeFM/o=";
      srcRoot = "tree-sitter-markdown-inline";
    }
    {
      name = "mlir";
      hash = "sha256-4Bde7dJgDdPkQrYBZ+bMfQKpGoqqB38dUcO9RijkfA4=";
      needs_generate = true;
    }
    { name = "yaml"; hash = "sha256-8z/jqz2XZ9dJgZQigki7TS0UYhSQGpcY84IE4B0HStQ="; }
    { name = "zig"; hash = "sha256-x69bGpkvyv/fUKEamXT7+PCdIMTZ70IkWskMFS3TqFo="; }
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
