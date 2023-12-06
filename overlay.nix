final: prev:
{
  mkTreesitter = final.callPackage ./nix/mkTreesitter.nix { };
  nvim-treesitter-lock-file = final.callPackage ./nix/nvim-treesitter-lock-file.nix { };
  nvim-treesitter-parsers = final.callPackage ./nix/nvim-treesitter-parsers.nix {
    wantedParsers = [
      { name = "bash"; hash = "sha256-b1r/T+Y4Kmui/pHsncozP8OO6rMMHJj+Xaa2Qzwo/cI="; }
      { name = "c"; hash = "sha256-sB8fNfusjC9yTlrizb2mufDzQPvBajTJC+ewF9awBqA="; }
      { name = "cpp"; hash = "sha256-27QjVy8quWyGhFCv/6GATG1xjGnkB9LTcvlPMuR3NB0="; }
      { name = "css"; hash = "sha256-vBm3z2x2m4pDPWzLIezkgqaKPGyTx4zQagJvDU6jVbY="; }
      { name = "diff"; hash = "sha256-0DMJCM0ps+oDyz4IzOPuI92lzDQMaq4trGos16WJQBc="; }
      { name = "firrtl"; hash = "sha256-X//iBrCi4sYgqNubUrnXCRoKBOUMsgS4u9yht7ioucA="; }
      { name = "gitcommit"; hash = "sha256-f7tSOL6/s+FAt3siH+eO63jXzbU79yh78QfHHvmBFbE="; }
      { name = "haskell"; hash = "sha256-emH6ZM/PndCQ3Co5KqC0B4i6m6UHikZB7SoZ4XpUnIs="; }
      { name = "javascript"; hash = "sha256-mQQHsSRwyQuXBLtPBj2kgwdtdlK8qFtEcIqG/2ogiY0="; }
      { name = "typescript"; hash = "sha256-xpXdkmodfLEljrqF/fZt/a6LFdfevi+FzzM5rixfB1E="; srcRoot = "typescript"; }
      { name = "tsx"; hash = "sha256-xpXdkmodfLEljrqF/fZt/a6LFdfevi+FzzM5rixfB1E="; srcRoot = "tsx"; }
      { name = "llvm"; hash = "sha256-c63jN6pyIssjthp+3f5pYWMwUq+usjhlP2lF/zVNdc8="; }
      { name = "lua"; hash = "sha256-ZocgN+GD7FOv/a2QuX8EoxwJ3MZCBnT2Y6Kv4jOvYy0="; }
      { name = "org"; hash = "sha256-N/zlpv4oXVfjk+a/7vM0nAPsCCBMVvWN3oavPbPmKwk="; }
      { name = "regex"; hash = "sha256-Y6A1YqbjItM4V5lQ7IM8EMa+nm6v+p/DHYSEVnF29ac="; }
      { name = "ruby"; hash = "sha256-RaxVKNoIaDj6tMi63ERmeRmq5yHlWL9/u2v6XpMsK/g="; }
      { name = "python"; hash = "sha256-2BW17L46CYrGISeSLWF8RrpAA0enEdJjlvuljnKDgLY="; }
      { name = "rust"; hash = "sha256-rwZbCa5f96BiqYWdbiHBRnlEU0TBJyycCoru0hxxu+U="; }
      { name = "scala"; hash = "sha256-2zmNRTey8cFrK9Kx4PrJnhMXFwX7HZj32GGeplJuiDc="; }
      { name = "nix"; hash = "sha256-rzrxcqcc7V+6pgdZ8Q/3VJd5/Oa58AtKKfoC3MBcirs="; }
      {
        name = "markdown";
        hash = "sha256-52QZ4bjJIvGxE4N4OJohdcyGSKjxep0pINJjgVq4H+M=";
        srcRoot = "tree-sitter-markdown";
      }
      {
        name = "markdown_inline";
        hash = "sha256-52QZ4bjJIvGxE4N4OJohdcyGSKjxep0pINJjgVq4H+M=";
        srcRoot = "tree-sitter-markdown-inline";
      }
      {
        name = "mlir";
        hash = "sha256-gSvZBwAvR93jfvcBLMBlKVQ/XOe9NXoh/Jzm8HRMcBI=";
        needs_generate = true;
      }
      {
        name = "ocaml";
        hash = "sha256-ovTvflpzfMvI+NaKtLfMyI/SyxPODqr4mRkfzROIEjc=";
        srcRoot = "ocaml";
      }
      {
        name = "ocaml_interface";
        hash = "sha256-ovTvflpzfMvI+NaKtLfMyI/SyxPODqr4mRkfzROIEjc=";
        srcRoot = "interface";
      }
      { name = "yaml"; hash = "sha256-RrYFKrhqFLsjQG+7XFbcQ2eYy2eyig5/r+MYO8DId4g="; }
    ];
  };
}
