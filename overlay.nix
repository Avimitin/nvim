final: prev:
{
  mkTreesitter = final.callPackage ./nix/mkTreesitter.nix { };
  fetchNvimTsLockFile = final.callPackage ./nix/nvim-treesitter-lock-file.nix { };
  generate-nvim-treesitter-parsers = final.callPackage ./nix/nvim-treesitter-parsers.nix { };

  treesitter-plugin-nightly = final.generate-nvim-treesitter-parsers [
    { name = "bash"; hash = "sha256-k8ZcAIgsdtKR9eAQzBJHALZkE1YF3bAeL6NQA5lYzAk="; }
    { name = "beancount"; hash = "sha256-aqz7S2CEWyzuSgqbsD4ZRfkAMzXGX+HbISAQi8hGHAY="; }
    { name = "c"; hash = "sha256-6jfE4VPJUsbCWtOJasanQq1Jnnol44KdUAFNKIpI6i8="; }
    { name = "cpp"; hash = "sha256-mSyx2Ar7+MAdKvnDLfvG+MbpKm/MisAmfufZjXAir2A="; }
    { name = "css"; hash = "sha256-MmXwp7V8waDG4+CM3gVCKWqQe4EvdaJiK+WUHQ0X0+k="; }
    { name = "comment"; hash = "sha256-XSkreGkg8yhTDPBPU2yZ8apXcEbNJ0csjv5xnp9vvwQ="; }
    { name = "diff"; hash = "sha256-LSiWJmB1uB0eVoDtCWktw6HW8LuBOJ0jnbs+je4nRh8="; }
    { name = "firrtl"; hash = "sha256-xrh8XlgcFz+Z1Rh7z1KZWV26RE65hVtIKjw9XPZaXuA="; }
    { name = "gitcommit"; hash = "sha256-5A3UHYdpI0+Uq03OWHTHXpqPHn618I1nw6IygEia9Fg="; }
    { name = "haskell"; hash = "sha256-yMRvKk3SXbvV1hXg8CQFYeni8dcnwRKJLNsPk6QMBCg="; }
    { name = "javascript"; hash = "sha256-odYqHvzAEnpVQHM8tIhbuemzOfOTfZXwmsxO8E33BtQ="; }
    { name = "typescript"; hash = "sha256-1m8y6mymrV9Hbnzo6ZN7rjjN2jgL6SB+gqGZ963gPCM="; srcRoot = "typescript"; }
    { name = "tsx"; hash = "sha256-1m8y6mymrV9Hbnzo6ZN7rjjN2jgL6SB+gqGZ963gPCM="; srcRoot = "tsx"; }
    { name = "typst"; hash = "sha256-cAG36OV1/qioKApZKZ8l1NsQ7FnXlXhi4Nq3KMpWvlA="; }
    { name = "llvm"; hash = "sha256-c63jN6pyIssjthp+3f5pYWMwUq+usjhlP2lF/zVNdc8="; }
    { name = "lua"; hash = "sha256-jGCiNmY35QYqWga5xOSvds0Vo9Kw6k/tTPD+pDBA8+c="; }
    { name = "org"; hash = "sha256-N/zlpv4oXVfjk+a/7vM0nAPsCCBMVvWN3oavPbPmKwk="; }
    { name = "regex"; hash = "sha256-QNQ6+/oOLmKp4fiz46LbeymSsN3xS4PYjsRPZC/2/yE="; }
    { name = "ruby"; hash = "sha256-tqI2z85mIymlrkiHwQcONs2cf1gcoqVVKkx0Fe4I+kw="; }
    { name = "python"; hash = "sha256-MS7AWXLq3GNlaG7s+yRH46c9Ycot1oTlDLiZdSCHkRE="; }
    { name = "rust"; hash = "sha256-fwlQb2pog3z8l+lvL2gGJsTURp3uhs8f3qjej+pYe6Y="; }
    { name = "proto"; hash = "sha256-eDnzT35wGxFzhcvy61d+1VG8ObB999mcakG3NNlrcck="; }
    { name = "scala"; hash = "sha256-wXwh8543+GWvE7I3pb8YyNQJVLikM2D5gbW+lGG+ovA="; }
    { name = "nix"; hash = "sha256-uiy+SPMzj/FVMa2pZYsz3kjsgSKNOdbxtYKa0ukXwPA="; }
    { name = "vimdoc"; hash = "sha256-iYGUNG16O/ggEnuc3UGvoVU71pzYd+hMk1gVqSVRZUY="; }
    { name = "query"; hash = "sha256-LS4ZNUqdF8WJej27Udi57vJJgac8eth51mwHNCUKTTs="; }
    {
      name = "markdown";
      hash = "sha256-JTfUM+RWZOGUvF3gERiK3JKENAzPRKnNfUrSDPZqDyY=";
      srcRoot = "tree-sitter-markdown";
    }
    {
      name = "markdown_inline";
      hash = "sha256-JTfUM+RWZOGUvF3gERiK3JKENAzPRKnNfUrSDPZqDyY=";
      srcRoot = "tree-sitter-markdown-inline";
    }
    {
      name = "mlir";
      hash = "sha256-uiqHQ3sBXY5Xs6Q1gt46LC5QcFlI1DlCcXBPLJv9AcE=";
      needs_generate = true;
    }
    { name = "yaml"; hash = "sha256-LcJOuP3ggn2AiosZbkxOFFASXfRrs3ytwrgX3/IdNrM="; }
    { name = "zig"; hash = "sha256-YzE9akXBp5rCCdvugUYtvkdmN160TrEhC+qpcYSxe1o="; }
  ];

  neovim-nightly-bin = final.neovim-unwrapped.overrideAttrs {
    # Disable default treesitter plugins, they are outdated
    treesitter-parsers = { };
  };
  neovim-nightly = final.wrapNeovim final.neovim-nightly-bin {
    extraMakeWrapperArgs = '' '--add-flags' '--cmd "set rtp^=${final.treesitter-plugin-nightly}"' '';
  };
}
