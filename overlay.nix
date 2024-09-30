final: prev:
{
  mkTreesitter = final.callPackage ./nix/mkTreesitter.nix { };
  fetchNvimTsLockFile = final.callPackage ./nix/nvim-treesitter-lock-file.nix { };
  generate-nvim-treesitter-parsers = final.callPackage ./nix/nvim-treesitter-parsers.nix { };

  treesitter-plugin-nightly = final.generate-nvim-treesitter-parsers [
    { name = "bash"; hash = "sha256-rEnP+u55Bi/SdhNbE/42iieC3Z2YxGJk8EvZQtboHr8="; }
    { name = "beancount"; hash = "sha256-aqz7S2CEWyzuSgqbsD4ZRfkAMzXGX+HbISAQi8hGHAY="; }
    { name = "c"; hash = "sha256-LVHguDVidPIMeYFxJwjm6Awza9K7RNT9NjSIauKeApA="; }
    { name = "cpp"; hash = "sha256-LM7IWki2RYHvU5qoB8NzbJfhnwTfd23RIBWofgOBu/E="; }
    { name = "css"; hash = "sha256-V81a7x+UteR4MpzlSrh9MhDwFLVTTStsGISclwvT1Fg="; }
    { name = "comment"; hash = "sha256-XSkreGkg8yhTDPBPU2yZ8apXcEbNJ0csjv5xnp9vvwQ="; }
    { name = "diff"; hash = "sha256-LSiWJmB1uB0eVoDtCWktw6HW8LuBOJ0jnbs+je4nRh8="; }
    { name = "firrtl"; hash = "sha256-xrh8XlgcFz+Z1Rh7z1KZWV26RE65hVtIKjw9XPZaXuA="; }
    { name = "gitcommit"; hash = "sha256-5A3UHYdpI0+Uq03OWHTHXpqPHn618I1nw6IygEia9Fg="; }
    { name = "haskell"; hash = "sha256-BpEmR0wZtnJ174/Pxe1VHc5lZ7Qetrqi33guqoQsNw0="; }
    { name = "javascript"; hash = "sha256-gvwc7jizdxRKd/isBm4fNhTRtLoW8Doo6DcIj0ANk7I="; }
    { name = "typescript"; hash = "sha256-9HdoVkqQiw+gixiUP7B6MdRPnklti7NkDOtyAyN9yao="; srcRoot = "typescript"; }
    { name = "tsx"; hash = "sha256-9HdoVkqQiw+gixiUP7B6MdRPnklti7NkDOtyAyN9yao="; srcRoot = "tsx"; }
    { name = "typst"; hash = "sha256-cAG36OV1/qioKApZKZ8l1NsQ7FnXlXhi4Nq3KMpWvlA="; }
    { name = "llvm"; hash = "sha256-c63jN6pyIssjthp+3f5pYWMwUq+usjhlP2lF/zVNdc8="; }
    { name = "lua"; hash = "sha256-angXXSWWgBaOaYC3jnLyhVoWU2RPPAeb/XNsIygcLzg="; }
    { name = "org"; hash = "sha256-N/zlpv4oXVfjk+a/7vM0nAPsCCBMVvWN3oavPbPmKwk="; }
    { name = "regex"; hash = "sha256-KaXH+rgL0hP4gjCayy7Sw8kSniPOwb6JKRZRXmwbE0I="; }
    { name = "ruby"; hash = "sha256-jiLrFFnwOQGXFR2pE4H6w5x2uDotBan+d2QfDjQUvWQ="; }
    { name = "python"; hash = "sha256-p7/qmxP8jdGPDDYRPmmO0gzzLqYl90GHi7N9oFwS4SM="; }
    { name = "rust"; hash = "sha256-S01KCcPKrrlIuF0qCNZZMeLzbQsvQYo+VgohURpLVBo="; }
    { name = "proto"; hash = "sha256-eDnzT35wGxFzhcvy61d+1VG8ObB999mcakG3NNlrcck="; }
    { name = "scala"; hash = "sha256-08xQCWiGq9Kb7JtOsA0YdHIF2Ue2cD0AUBEz9mGtBII="; }
    { name = "nix"; hash = "sha256-wmqnG3/5qWX4Yk3fjfF8EkdXcqjBqly8C85EAxrnXf0="; }
    { name = "vimdoc"; hash = "sha256-iYGUNG16O/ggEnuc3UGvoVU71pzYd+hMk1gVqSVRZUY="; }
    { name = "query"; hash = "sha256-LS4ZNUqdF8WJej27Udi57vJJgac8eth51mwHNCUKTTs="; }
    {
      name = "markdown";
      hash = "sha256-vZ1ybDTt9O+gu5YvybbITv7EwM5C6Jhn8k5SF/L8vmY=";
      srcRoot = "tree-sitter-markdown";
    }
    {
      name = "markdown_inline";
      hash = "sha256-vZ1ybDTt9O+gu5YvybbITv7EwM5C6Jhn8k5SF/L8vmY=";
      srcRoot = "tree-sitter-markdown-inline";
    }
    {
      name = "mlir";
      hash = "sha256-U6B944pd1rt6itlrF/M9zuXlB73QfTeZmCXMCghuYqQ=";
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
