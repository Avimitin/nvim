final: prev:
{
  mkTreesitter = final.callPackage ./nix/mkTreesitter.nix { };
  fetchNvimTsLockFile = final.callPackage ./nix/nvim-treesitter-lock-file.nix { };
  generate-nvim-treesitter-parsers = final.callPackage ./nix/nvim-treesitter-parsers.nix { };

  treesitter-plugin-nightly = final.generate-nvim-treesitter-parsers [
    { name = "bash"; hash = "sha256-k8ZcAIgsdtKR9eAQzBJHALZkE1YF3bAeL6NQA5lYzAk="; }
    { name = "beancount"; hash = "sha256-aqz7S2CEWyzuSgqbsD4ZRfkAMzXGX+HbISAQi8hGHAY="; }
    { name = "c"; hash = "sha256-SJnTA9nNmILobU7TK9NdifpxHraD0DtW/jhaUl4AMsQ="; }
    { name = "cpp"; hash = "sha256-4IjToK03tjCPVT/Ay82FOER/mq9Nqvm4HAPt+jdAAAQ="; }
    { name = "css"; hash = "sha256-J8Mhaw4H945yxGTYDVQSnhHHVQH8f7jmXdwcPdoJGV8="; }
    { name = "comment"; hash = "sha256-XLsWZ8ClsWmHgiMnSGWlMWAs8dSkIX86/hRcCTD/7/o="; }
    { name = "diff"; hash = "sha256-LSiWJmB1uB0eVoDtCWktw6HW8LuBOJ0jnbs+je4nRh8="; }
    { name = "firrtl"; hash = "sha256-xrh8XlgcFz+Z1Rh7z1KZWV26RE65hVtIKjw9XPZaXuA="; }
    { name = "gitcommit"; hash = "sha256-5A3UHYdpI0+Uq03OWHTHXpqPHn618I1nw6IygEia9Fg="; }
    { name = "haskell"; hash = "sha256-yMRvKk3SXbvV1hXg8CQFYeni8dcnwRKJLNsPk6QMBCg="; }
    { name = "javascript"; hash = "sha256-Wd38cAuAfaJSCEVmntt2uV6dkVXXq8BzDywlwWZSVwY="; }
    { name = "typescript"; hash = "sha256-1m8y6mymrV9Hbnzo6ZN7rjjN2jgL6SB+gqGZ963gPCM="; srcRoot = "typescript"; }
    { name = "tsx"; hash = "sha256-1m8y6mymrV9Hbnzo6ZN7rjjN2jgL6SB+gqGZ963gPCM="; srcRoot = "tsx"; }
    { name = "typst"; hash = "sha256-cAG36OV1/qioKApZKZ8l1NsQ7FnXlXhi4Nq3KMpWvlA="; }
    { name = "llvm"; hash = "sha256-c63jN6pyIssjthp+3f5pYWMwUq+usjhlP2lF/zVNdc8="; }
    { name = "lua"; hash = "sha256-jGCiNmY35QYqWga5xOSvds0Vo9Kw6k/tTPD+pDBA8+c="; }
    { name = "org"; hash = "sha256-N/zlpv4oXVfjk+a/7vM0nAPsCCBMVvWN3oavPbPmKwk="; }
    { name = "regex"; hash = "sha256-QNQ6+/oOLmKp4fiz46LbeymSsN3xS4PYjsRPZC/2/yE="; }
    { name = "ruby"; hash = "sha256-4XF+HD5TKL5JQuPavUPyaN810JSkI5jEOZ2tK7dpP1I="; }
    { name = "python"; hash = "sha256-i+oyYv9NHzU5xgrqsyH2qUIOkNmp8CAkDvNgYetRnAo="; }
    { name = "rust"; hash = "sha256-ngq5h8pXVOZIB6h04oxY6Hmte9TBffTUrIKeZvQ2fXU="; }
    { name = "proto"; hash = "sha256-eDnzT35wGxFzhcvy61d+1VG8ObB999mcakG3NNlrcck="; }
    { name = "scala"; hash = "sha256-YKqRo1hD3P1pW6cPIJtUzAwwiN6MIbCd1I2lkbYEa2Q="; }
    { name = "nix"; hash = "sha256-JuaJZvyj35F99L4kE1tBHD4tVyg5lSycG3CPhSMk/TI="; }
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
