final: prev:
{
  mkTreesitter = final.callPackage ./nix/mkTreesitter.nix { };
  fetchNvimTsLockFile = final.callPackage ./nix/nvim-treesitter-lock-file.nix { };
  generate-nvim-treesitter-parsers = final.callPackage ./nix/nvim-treesitter-parsers.nix { };

  treesitter-plugin-nightly = final.generate-nvim-treesitter-parsers [
    { name = "bash"; hash = "sha256-k8ZcAIgsdtKR9eAQzBJHALZkE1YF3bAeL6NQA5lYzAk="; }
    { name = "beancount"; hash = "sha256-4KQUwv3PDJPBChLhDSXJpKNUrzUPOk1o9Bs6Z2Tq8+Q="; }
    { name = "c"; hash = "sha256-EHFWT6lGAW8B7nm6sktetjJP+bONjnKYfhWUNZal3Ck="; }
    { name = "cpp"; hash = "sha256-MbPFaljqpHbOXkN31CNOHTJmYPAWmPxy6P/FUmix4q8="; }
    { name = "css"; hash = "sha256-J8Mhaw4H945yxGTYDVQSnhHHVQH8f7jmXdwcPdoJGV8="; }
    { name = "comment"; hash = "sha256-XLsWZ8ClsWmHgiMnSGWlMWAs8dSkIX86/hRcCTD/7/o="; }
    { name = "diff"; hash = "sha256-O0ProK/Nu8QFqdMoSzr3MzL6NSL4iKhin40U1oXml4E="; }
    { name = "firrtl"; hash = "sha256-xrh8XlgcFz+Z1Rh7z1KZWV26RE65hVtIKjw9XPZaXuA="; }
    { name = "gitcommit"; hash = "sha256-j+W3wmSKTEXAB/LyOnBvzNx4CptQ+JplJ3IVQzs86ss="; }
    { name = "haskell"; hash = "sha256-yMRvKk3SXbvV1hXg8CQFYeni8dcnwRKJLNsPk6QMBCg="; }
    { name = "javascript"; hash = "sha256-9iG3PMraJpBctU8SHsB6xazAxL/sdqp6A92HIsO4dE0="; }
    { name = "typescript"; hash = "sha256-C1xW/kCxmwzX9HjKwaM3zn3mWwsOhhfW+97/QarF3Kk="; srcRoot = "typescript"; }
    { name = "tsx"; hash = "sha256-C1xW/kCxmwzX9HjKwaM3zn3mWwsOhhfW+97/QarF3Kk="; srcRoot = "tsx"; }
    { name = "typst"; hash = "sha256-amBpZ4xw+OuBMbyftDzCmbrYQTz8iuhcbH7nD4j7BVs="; }
    { name = "llvm"; hash = "sha256-c63jN6pyIssjthp+3f5pYWMwUq+usjhlP2lF/zVNdc8="; }
    { name = "lua"; hash = "sha256-jGCiNmY35QYqWga5xOSvds0Vo9Kw6k/tTPD+pDBA8+c="; }
    { name = "org"; hash = "sha256-N/zlpv4oXVfjk+a/7vM0nAPsCCBMVvWN3oavPbPmKwk="; }
    { name = "regex"; hash = "sha256-QNQ6+/oOLmKp4fiz46LbeymSsN3xS4PYjsRPZC/2/yE="; }
    { name = "ruby"; hash = "sha256-k8oIFPexbK9Lo2sKV3tC1I+4fOeXn83gHEcG/aUTZjQ="; }
    { name = "python"; hash = "sha256-OgPTwM8vFuNkmzJbgTn3i5Svx8469OHdrBlmvXNhiwI="; }
    { name = "rust"; hash = "sha256-ngq5h8pXVOZIB6h04oxY6Hmte9TBffTUrIKeZvQ2fXU="; }
    { name = "proto"; hash = "sha256-eDnzT35wGxFzhcvy61d+1VG8ObB999mcakG3NNlrcck="; }
    { name = "scala"; hash = "sha256-EhVojG0RKh+44cnHUSKGa97Jw5eeIcSndL+zFnTD7Nk="; }
    { name = "nix"; hash = "sha256-pockUWVZUWcfRZ+p0+TkhLlp1YMppBHYJVy/TnEy+pc="; }
    { name = "vimdoc"; hash = "sha256-6sFzc6DwJrzlRVpKIdhYqx4RtAplpvkAlB51ogDOkbE="; }
    { name = "query"; hash = "sha256-2RMTcm9Myn1Xz8PB3qvUGMvTMY0QUCOFZC6q/OkoXCs="; }
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
      hash = "sha256-bkbo0ZL/1/qI1ole9ytcLDvswxq38Fw7ZD75RWPXF1A=";
      needs_generate = true;
    }
    { name = "yaml"; hash = "sha256-LcJOuP3ggn2AiosZbkxOFFASXfRrs3ytwrgX3/IdNrM="; }
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
