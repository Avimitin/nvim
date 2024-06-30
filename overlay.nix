final: prev:
{
  mkTreesitter = final.callPackage ./nix/mkTreesitter.nix { };
  fetchNvimTsLockFile = final.callPackage ./nix/nvim-treesitter-lock-file.nix { };
  generate-nvim-treesitter-parsers = final.callPackage ./nix/nvim-treesitter-parsers.nix { };

  treesitter-plugin-nightly = final.generate-nvim-treesitter-parsers [
    { name = "bash"; hash = "sha256-k8ZcAIgsdtKR9eAQzBJHALZkE1YF3bAeL6NQA5lYzAk="; }
    { name = "beancount"; hash = "sha256-4KQUwv3PDJPBChLhDSXJpKNUrzUPOk1o9Bs6Z2Tq8+Q="; }
    { name = "c"; hash = "sha256-S/mr958IwtbZ2mXMNlHlMhZsapy9w2tc0/xJSYoxnp4="; }
    { name = "cpp"; hash = "sha256-E4u+PGSPHdVdB+xzroImzDblLsN10s6k0Pf3HLdmnU4="; }
    { name = "css"; hash = "sha256-J8Mhaw4H945yxGTYDVQSnhHHVQH8f7jmXdwcPdoJGV8="; }
    { name = "comment"; hash = "sha256-XLsWZ8ClsWmHgiMnSGWlMWAs8dSkIX86/hRcCTD/7/o="; }
    { name = "diff"; hash = "sha256-O0ProK/Nu8QFqdMoSzr3MzL6NSL4iKhin40U1oXml4E="; }
    { name = "firrtl"; hash = "sha256-xrh8XlgcFz+Z1Rh7z1KZWV26RE65hVtIKjw9XPZaXuA="; }
    { name = "gitcommit"; hash = "sha256-j+W3wmSKTEXAB/LyOnBvzNx4CptQ+JplJ3IVQzs86ss="; }
    { name = "haskell"; hash = "sha256-yMRvKk3SXbvV1hXg8CQFYeni8dcnwRKJLNsPk6QMBCg="; }
    { name = "javascript"; hash = "sha256-LYONLr4/Y0rFzp3/JK4ybHe8JfsODUIk+ijNllCZ6Y8="; }
    { name = "typescript"; hash = "sha256-wwvYo5+bQWIxIK9lA+kSyBxedIhc2N+D5bMrqUdnMyM="; srcRoot = "typescript"; }
    { name = "tsx"; hash = "sha256-wwvYo5+bQWIxIK9lA+kSyBxedIhc2N+D5bMrqUdnMyM="; srcRoot = "tsx"; }
    { name = "typst"; hash = "sha256-amBpZ4xw+OuBMbyftDzCmbrYQTz8iuhcbH7nD4j7BVs="; }
    { name = "llvm"; hash = "sha256-c63jN6pyIssjthp+3f5pYWMwUq+usjhlP2lF/zVNdc8="; }
    { name = "lua"; hash = "sha256-jGCiNmY35QYqWga5xOSvds0Vo9Kw6k/tTPD+pDBA8+c="; }
    { name = "org"; hash = "sha256-N/zlpv4oXVfjk+a/7vM0nAPsCCBMVvWN3oavPbPmKwk="; }
    { name = "regex"; hash = "sha256-QNQ6+/oOLmKp4fiz46LbeymSsN3xS4PYjsRPZC/2/yE="; }
    { name = "ruby"; hash = "sha256-4XF+HD5TKL5JQuPavUPyaN810JSkI5jEOZ2tK7dpP1I="; }
    { name = "python"; hash = "sha256-OgPTwM8vFuNkmzJbgTn3i5Svx8469OHdrBlmvXNhiwI="; }
    { name = "rust"; hash = "sha256-ngq5h8pXVOZIB6h04oxY6Hmte9TBffTUrIKeZvQ2fXU="; }
    { name = "proto"; hash = "sha256-eDnzT35wGxFzhcvy61d+1VG8ObB999mcakG3NNlrcck="; }
    { name = "scala"; hash = "sha256-qkany4Eb0Sw0svV2h1/Y/GJj8wUUH7XvYYr3uV6S1vE="; }
    { name = "nix"; hash = "sha256-drRtS9oymO3HQpJaojAQIvHk01ni1ix/1eaqfbQTQz0="; }
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
      hash = "sha256-fV6W4n2/SxGrmJ/7dc3cS6trxmdepGF7az23oSKjifg=";
      needs_generate = true;
    }
    { name = "yaml"; hash = "sha256-LcJOuP3ggn2AiosZbkxOFFASXfRrs3ytwrgX3/IdNrM="; }
    { name = "zig"; hash = "sha256-qmOPg9G3OjyYlubhyWzUGBqPylrUDX3tnC4Aprq6qZE="; }
  ];

  neovim-nightly-bin = final.neovim-unwrapped.overrideAttrs {
    version = "0.10.0";
    src = final.fetchFromGitHub {
      repo = "neovim";
      owner = "neovim";
      rev = "v0.10.0";
      hash = "sha256-FCOipXHkAbkuFw9JjEpOIJ8BkyMkjkI0Dp+SzZ4yZlw=";
    };

    # Disable default treesitter plugins, they are outdated
    treesitter-parsers = { };
  };
  neovim-nightly = final.wrapNeovim final.neovim-nightly-bin {
    extraMakeWrapperArgs = ''
      --add-flags "--cmd 'set rtp^=${final.treesitter-plugin-nightly}'"
    '';
  };
}
