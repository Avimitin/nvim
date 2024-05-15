final: prev:
{
  mkTreesitter = final.callPackage ./nix/mkTreesitter.nix { };
  fetchNvimTsLockFile = final.callPackage ./nix/nvim-treesitter-lock-file.nix { };
  generate-nvim-treesitter-parsers = final.callPackage ./nix/nvim-treesitter-parsers.nix { };

  treesitter-plugin-nightly = final.generate-nvim-treesitter-parsers [
    { name = "bash"; hash = "sha256-24tPIgyB94eZufGv5BNtDpwj3+K9nbqTJzW7wI12mqY="; }
    { name = "beancount"; hash = "sha256-4KQUwv3PDJPBChLhDSXJpKNUrzUPOk1o9Bs6Z2Tq8+Q="; }
    { name = "c"; hash = "sha256-PMyML+nO9q8G2vpmRKmr0xbBqvO1LW+jIMtrFBaZZ9g="; }
    { name = "cpp"; hash = "sha256-G0hXG83d6MiSXklh5f1Xncmq4mTGtlm4a6JtlRCqvlA="; }
    { name = "css"; hash = "sha256-wu+mjyBNvIgZAEL36CqXQntbGq9qizQUzhzb86bKSU8="; }
    { name = "diff"; hash = "sha256-K4ybmnit3POCWRw8JmS48DyfZXQ7BV0/e7yn6jlg6ss="; }
    { name = "firrtl"; hash = "sha256-xrh8XlgcFz+Z1Rh7z1KZWV26RE65hVtIKjw9XPZaXuA="; }
    { name = "gitcommit"; hash = "sha256-j+W3wmSKTEXAB/LyOnBvzNx4CptQ+JplJ3IVQzs86ss="; }
    { name = "haskell"; hash = "sha256-BTGCx4ZtEBRunCUGufoiPBZ6U0AhGpT56mOQ37UwvoA="; }
    { name = "javascript"; hash = "sha256-Wcg6hzSJHFMApKs82vYlrAP1wVF1XKHgyCg9JfFpMBU="; }
    { name = "typescript"; hash = "sha256-9pvCu2u1+9zCtupDaDZKnIr5ZW58EtKs4VOX6OuRCqA="; srcRoot = "typescript"; }
    { name = "tsx"; hash = "sha256-9pvCu2u1+9zCtupDaDZKnIr5ZW58EtKs4VOX6OuRCqA="; srcRoot = "tsx"; }
    { name = "typst"; hash = "sha256-cMLiXG/74s6PIMXtZfuuKryw34PqBbaT4Ahq1n3w/WY="; }
    { name = "llvm"; hash = "sha256-c63jN6pyIssjthp+3f5pYWMwUq+usjhlP2lF/zVNdc8="; }
    { name = "lua"; hash = "sha256-jGCiNmY35QYqWga5xOSvds0Vo9Kw6k/tTPD+pDBA8+c="; }
    { name = "org"; hash = "sha256-N/zlpv4oXVfjk+a/7vM0nAPsCCBMVvWN3oavPbPmKwk="; }
    { name = "regex"; hash = "sha256-4NHC4z07lBOhmBABNJqpBYds3P6v1mAY9/i0+MuKeuc="; }
    { name = "ruby"; hash = "sha256-8ooaR58y9jCtQJ2oKIw3ZESG7rzCjrUNeBSdm8SC1jU="; }
    { name = "python"; hash = "sha256-zWcb3Of6dB1PF1OwwrWSJ7z+WvmMnruaum49J5x1+DI="; }
    { name = "rust"; hash = "sha256-E2h5wH4tei4oUt/Bp+26+JQqPauBIv0e9V/8r/CWroM="; }
    { name = "proto"; hash = "sha256-eDnzT35wGxFzhcvy61d+1VG8ObB999mcakG3NNlrcck="; }
    { name = "scala"; hash = "sha256-EhVojG0RKh+44cnHUSKGa97Jw5eeIcSndL+zFnTD7Nk="; }
    { name = "nix"; hash = "sha256-pockUWVZUWcfRZ+p0+TkhLlp1YMppBHYJVy/TnEy+pc="; }
    { name = "vimdoc"; hash = "sha256-6sFzc6DwJrzlRVpKIdhYqx4RtAplpvkAlB51ogDOkbE="; }
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
      hash = "sha256-W95vLOnH54jm23J8zsVbtPj9SzuqORDrw1vzYjGWmp4=";
      needs_generate = true;
    }
    { name = "yaml"; hash = "sha256-tZgjmjT1jR3LfuJYDYysVr3MxTnhLlYRRtePVU643Bw="; }
  ];

  neovim-nightly-bin =
    let
      srcInfo = final.callPackage ./nix/_sources/generated.nix { };
    in
    final.neovim-unwrapped.overrideAttrs {
      inherit (srcInfo.neovim-nightly) version src;
    };
  neovim-nightly = final.wrapNeovim final.neovim-nightly-bin {
    extraMakeWrapperArgs = ''
      --add-flags "-c 'set rtp^=${final.treesitter-plugin-nightly}'"
    '';
  };
}
