final: prev:
{
  mkTreesitter = final.callPackage ./nix/mkTreesitter.nix { };
  fetchNvimTsLockFile = final.callPackage ./nix/nvim-treesitter-lock-file.nix { };
  generate-nvim-treesitter-parsers = final.callPackage ./nix/nvim-treesitter-parsers.nix { };

  treesitter-plugin-nightly = final.generate-nvim-treesitter-parsers [
    { name = "bash"; hash = "sha256-FEJtt+wHOj10lo17CHKiGqjyuY6qcfmOk5LWccaG8pY="; }
    { name = "c"; hash = "sha256-Nxs5e61F72OPZOQQdN6lMeECIVFfnR1vMBkaNOWWTY0="; }
    { name = "cpp"; hash = "sha256-/WmK292MwOrBEmuHkqFVwiJCZ/I0ZKlIoN85dNKBgu4="; }
    { name = "css"; hash = "sha256-cNI01V03nmkkhwRAOb1rrWCf6oHnO1jBE0QJUFIfc3c="; }
    { name = "comment"; hash = "sha256-XSkreGkg8yhTDPBPU2yZ8apXcEbNJ0csjv5xnp9vvwQ="; }
    { name = "diff"; hash = "sha256-r2DgtdvwLWPYGG3Rn8WPAxz2JQP3CMNFRfrvToceZTc="; }
    { name = "firrtl"; hash = "sha256-xrh8XlgcFz+Z1Rh7z1KZWV26RE65hVtIKjw9XPZaXuA="; }
    { name = "gitcommit"; hash = "sha256-DDBJYsJNQggyuW2IsB2zjDPEqen7mKhe7Xj5o0DwUWQ="; }
    { name = "haskell"; hash = "sha256-embdahpMEk2qSf6xyZPH1Xt8PzSdQzP/0gdnfzNSCKo="; }
    { name = "javascript"; hash = "sha256-xY40F/RpZUS8Wj6AoOOM5zU6YC26kQoV3uv9cjPjn68="; }
    { name = "typescript"; hash = "sha256-nuKJh4uuuCRn0c2OHN5Slysc4N/MxiFHe7FaWF8jckA="; srcRoot = "typescript"; }
    { name = "tsx"; hash = "sha256-nuKJh4uuuCRn0c2OHN5Slysc4N/MxiFHe7FaWF8jckA="; srcRoot = "tsx"; }
    { name = "typst"; hash = "sha256-w9ZOaH3SY151yQs1jDTymqS1jB4RlCxSvUbko3SpHlE="; }
    { name = "llvm"; hash = "sha256-3/WPq64L+L0ewbrbvihkeuLOCoS7XhiKQnFIwS3i4HY="; }
    { name = "lua"; hash = "sha256-bWW1MZQukBCTRGxBHXZY0vQ8MwFrft8GuNDatfPhYDI="; }
    { name = "ocaml"; hash = "sha256-hHke8TgP+ypIbOrHJLDjBGpANYX03PJ4T1o+5K/Th0k="; srcRoot = "grammars/ocaml"; }
    { name = "ocaml_interface"; hash = "sha256-hHke8TgP+ypIbOrHJLDjBGpANYX03PJ4T1o+5K/Th0k="; srcRoot = "grammars/interface"; }
    { name = "regex"; hash = "sha256-fcrreGW8NdpI/8fYZS9M/kh2VfZC25L02uQLGNO7yy0="; }
    { name = "ruby"; hash = "sha256-Ai3S2y77OWyUnTWnK9lEMWFI6wDL0b1bYpRW9xDhfmE="; }
    { name = "python"; hash = "sha256-Ep0vTEyQY0bFf3+dc57RZhJdD4auLPj3nV6cZiHXR6s="; }
    { name = "rust"; hash = "sha256-/KpfyAVSEuZj9BzlZYPsmpWojrOL3ZY0O295HiayZps="; }
    { name = "proto"; hash = "sha256-eDnzT35wGxFzhcvy61d+1VG8ObB999mcakG3NNlrcck="; }
    { name = "scala"; hash = "sha256-6WbaJwcIWBVvDzmnbFUpTOG4rDrOR+rUWwe5Eed5ieQ="; }
    { name = "nix"; hash = "sha256-bPMcTMr4oTolb0FQfRB+i6hnipn9cH2sjl4JD/+0kOs="; }
    { name = "vimdoc"; hash = "sha256-zcUVFK2CCGiHk7jfx2QwXmjOAApBnM0Q9HyMMiDhCHU="; }
    { name = "query"; hash = "sha256-LS4ZNUqdF8WJej27Udi57vJJgac8eth51mwHNCUKTTs="; }
    {
      name = "markdown";
      hash = "sha256-Nl+4nSkRrGjZKb7TD+J/MfdzhYja5t2pxrEIANWEKTI=";
      srcRoot = "tree-sitter-markdown";
    }
    {
      name = "markdown_inline";
      hash = "sha256-Nl+4nSkRrGjZKb7TD+J/MfdzhYja5t2pxrEIANWEKTI=";
      srcRoot = "tree-sitter-markdown-inline";
    }
    {
      name = "mlir";
      hash = "sha256-p+ayKSrapFE9p5xxVFaSSzeZv18DDEb+HdVOmsIyi6o=";
      needs_generate = true;
    }
    { name = "yaml"; hash = "sha256-LcJOuP3ggn2AiosZbkxOFFASXfRrs3ytwrgX3/IdNrM="; }
    { name = "zig"; hash = "sha256-y9fmepjJ0tfk0o4uh/SWViXYZHlb9bRiQb+Dn6rpXyw="; }
  ];

  neovim-nightly-bin = final.neovim-unwrapped.overrideAttrs {
    # Disable default treesitter plugins, they are outdated
    treesitter-parsers = { };
  };
  neovim-nightly = final.wrapNeovim final.neovim-nightly-bin {
    extraMakeWrapperArgs = '' '--add-flags' '--cmd "set rtp^=${final.treesitter-plugin-nightly}"' '';
  };
}
