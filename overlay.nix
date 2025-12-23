final: prev: {
  mkTreesitter = final.callPackage ./nix/mkTreesitter.nix { };
  fetchNvimTsLockFile = final.callPackage ./nix/nvim-treesitter-lock-file.nix { };
  generate-nvim-treesitter-parsers = final.callPackage ./nix/nvim-treesitter-parsers.nix { };

  treesitter-plugin-nightly = final.generate-nvim-treesitter-parsers [
    {
      name = "bash";
      hash = "sha256-aYOHtNMKMmctxI1q9CGmid17kUHSWWOActiMlSRVmc0=";
    }
    {
      name = "c";
      hash = "sha256-t5zse6CwEz99H0BjQLKURmr1FMPhoEJVXtDMjVzsXSk=";
    }
    {
      name = "cpp";
      hash = "sha256-gVS1uEK3pB+Izmgxc41nQ69i5MhVmM+dL4A4Tw81a+A=";
    }
    {
      name = "css";
      hash = "sha256-gLLd4X3sGo+dzwa+sMouT4GJIsAv0x4vxxjKQbJQrxs=";
    }
    {
      name = "comment";
      hash = "sha256-XSkreGkg8yhTDPBPU2yZ8apXcEbNJ0csjv5xnp9vvwQ=";
    }
    {
      name = "diff";
      hash = "sha256-Q61DD6CX81M3npBflPE5P3glMdfCNB877juXFgKqu7Q=";
    }
    {
      name = "firrtl";
      hash = "sha256-xrh8XlgcFz+Z1Rh7z1KZWV26RE65hVtIKjw9XPZaXuA=";
    }
    {
      name = "gitcommit";
      hash = "sha256-pthd5ATiFnrSwY7OCgK5SSaj5PII1y+sbq3VL572OZg=";
    }
    {
      name = "haskell";
      hash = "sha256-R9w8P29Heh0JtTT534e++Gpa315XN92hlQw2A8pRTpI=";
    }
    {
      name = "javascript";
      hash = "sha256-SnIKLaKCIflGh4Pie+pMG6CYPxiEe8cu6juMcBhfJpg=";
    }
    {
      name = "typescript";
      hash = "sha256-ls5NG1E3Z9QUvMpAjv2bSYeRYszuzb7XmojorSGE84U=";
      srcRoot = "typescript";
    }
    {
      name = "tsx";
      hash = "sha256-ls5NG1E3Z9QUvMpAjv2bSYeRYszuzb7XmojorSGE84U=";
      srcRoot = "tsx";
    }
    {
      name = "typst";
      hash = "sha256-BnSyu5q5IPf6g/+2pCoyDfFLeD+8fKvSXXFQOGXnzUs=";
    }
    {
      name = "llvm";
      hash = "sha256-3/WPq64L+L0ewbrbvihkeuLOCoS7XhiKQnFIwS3i4HY=";
    }
    {
      name = "lua";
      hash = "sha256-0gK6jswRs0lMIuGGgRwFnQAjZH3xSmATzC1cERLG000=";
    }
    {
      name = "ocaml";
      hash = "sha256-l8k6+QLuXplP09aNzJyzkvdThlduJd7wskGCZ9qN2gs=";
      srcRoot = "grammars/ocaml";
    }
    {
      name = "ocaml_interface";
      hash = "sha256-l8k6+QLuXplP09aNzJyzkvdThlduJd7wskGCZ9qN2gs=";
      srcRoot = "grammars/interface";
    }
    {
      name = "regex";
      hash = "sha256-Os29dXqV12QcgrEjj+to55cprPUmxxYXW0rBjkb2ETw=";
    }
    {
      name = "ruby";
      hash = "sha256-jsEbPD+PIcr281nqxtonYYID/SadnSlARUpbhVy3GAM=";
    }
    {
      name = "python";
      hash = "sha256-MF7HdaUCl2r1gepj/+OeHW8WYXGRMdsSBLtxgto4ZEI=";
    }
    {
      name = "rust";
      hash = "sha256-U2Viwf80W7UeMViQPpr8v16g9JY6N1mP4ezYq0cYpJc=";
    }
    {
      name = "proto";
      hash = "sha256-eDnzT35wGxFzhcvy61d+1VG8ObB999mcakG3NNlrcck=";
    }
    {
      name = "scala";
      hash = "sha256-iDxcht6pRtJc/MTg1CbF5x36pdZVgtZqWQCDigO4ymk=";
    }
    {
      name = "nix";
      hash = "sha256-ZReufsys08Xh7BQFhYOc7FLtz9P1HUcUP5VD4pk5hUM=";
    }
    {
      name = "vimdoc";
      hash = "sha256-zCEusqf9IKsfp1nUSu6CPQEOs+A9bOQk9i++cAiQy8o=";
    }
    {
      name = "query";
      hash = "sha256-ZLbsbuf+8kikkdsiVRmkLBDBKYwLonJtjKtojpAHzRg=";
    }
    {
      name = "markdown";
      hash = "sha256-T6RAgjmBVn0uSG9KG4XBZ4Gt+YPAZqIhac/NcnPGebM=";
      srcRoot = "tree-sitter-markdown";
    }
    {
      name = "markdown_inline";
      hash = "sha256-T6RAgjmBVn0uSG9KG4XBZ4Gt+YPAZqIhac/NcnPGebM=";
      srcRoot = "tree-sitter-markdown-inline";
    }
    {
      name = "mlir";
      hash = "sha256-sXYSc5+yD3odBVJQG0JTK9Nihe13oxSox6kzllq2jss=";
      needs_generate = true;
    }
    {
      name = "yaml";
      hash = "sha256-FMTYaOpWez3Tr3ojFsh9XjE62F9h+6TKewnGGd8zv0I=";
    }
    {
      name = "zig";
      hash = "sha256-x69bGpkvyv/fUKEamXT7+PCdIMTZ70IkWskMFS3TqFo=";
    }
    {
      name = "meson";
      hash = "sha256-nAVOjkwoPK7AYSWqqzMynBCfovchM8cbbYv/ArtSj8g=";
    }
  ];

  neovim-nightly-bin = final.neovim-unwrapped.overrideAttrs {
    # Disable default treesitter plugins, they are outdated
    treesitter-parsers = { };
  };
  neovim-nightly = final.wrapNeovim final.neovim-nightly-bin {
    extraMakeWrapperArgs = '''--add-flags' '--cmd "set rtp^=${final.treesitter-plugin-nightly}"' '';
  };

  ghc-for-ts-plugins = final.haskellPackages.ghcWithPackages (
    pkgs: with pkgs; [
      aeson
      turtle
    ]
  );
}
