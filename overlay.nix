final: prev:
{
  mkTreesitter = final.callPackage ./nix/mkTreesitter.nix { };
  nvim-treesitter-lock-file = final.callPackage ./nix/nvim-treesitter-lock-file.nix { };
  nvim-treesitter-parsers = final.callPackage ./nix/nvim-treesitter-parsers.nix {
    wantedParsers = [
      { name = "bash"; hash = "sha256-QQmgtC/1/8ps3tPl9X3z/sVQSlGW5h+DC364LBjLbWQ="; }
      { name = "c"; hash = "sha256-h2ucwhwxTmv/RB/j5OwUbS6M18dNvmHqpYKrnP2pEMQ="; }
      { name = "cpp"; hash = "sha256-WGGrXFiSFUsha4Xz48MD8wEGXznGNG5E7CPdOZPCJ/Y="; }
      { name = "css"; hash = "sha256-AaOsj7Ia89R9f2gRxFq+9Y3pam+4eYbsU6Yd4+N3b6Q="; }
      { name = "diff"; hash = "sha256-0DMJCM0ps+oDyz4IzOPuI92lzDQMaq4trGos16WJQBc="; }
      { name = "firrtl"; hash = "sha256-X//iBrCi4sYgqNubUrnXCRoKBOUMsgS4u9yht7ioucA="; }
      { name = "gitcommit"; hash = "sha256-f7tSOL6/s+FAt3siH+eO63jXzbU79yh78QfHHvmBFbE="; }
      { name = "javascript"; hash = "sha256-mQQHsSRwyQuXBLtPBj2kgwdtdlK8qFtEcIqG/2ogiY0="; }
      { name = "typescript"; hash = "sha256-wgFce0+8TA9gmvcuNg5YNhySuEzt8ZF/nrHPmwFZW14="; srcRoot = "typescript"; }
      { name = "tsx"; hash = "sha256-wgFce0+8TA9gmvcuNg5YNhySuEzt8ZF/nrHPmwFZW14="; srcRoot = "tsx"; }
      { name = "llvm"; hash = "sha256-c63jN6pyIssjthp+3f5pYWMwUq+usjhlP2lF/zVNdc8="; }
      { name = "lua"; hash = "sha256-ZocgN+GD7FOv/a2QuX8EoxwJ3MZCBnT2Y6Kv4jOvYy0="; }
      { name = "regex"; hash = "sha256-Y6A1YqbjItM4V5lQ7IM8EMa+nm6v+p/DHYSEVnF29ac="; }
      { name = "ruby"; hash = "sha256-RaxVKNoIaDj6tMi63ERmeRmq5yHlWL9/u2v6XpMsK/g="; }
      { name = "rust"; hash = "sha256-g/AJGKg/8KGgcIJZChb9cIP/zvS1JIcUEZRxBL0x2nY="; }
      { name = "scala"; hash = "sha256-2zmNRTey8cFrK9Kx4PrJnhMXFwX7HZj32GGeplJuiDc="; }
      { name = "nix"; hash = "sha256-rzrxcqcc7V+6pgdZ8Q/3VJd5/Oa58AtKKfoC3MBcirs="; }
      {
        name = "markdown";
        hash = "sha256-GSuepOjwSCfWmlFZ3YnnzaaC/fzr4+kNttw97BmMOsE=";
        srcRoot = "tree-sitter-markdown";
      }
      {
        name = "markdown_inline";
        hash = "sha256-GSuepOjwSCfWmlFZ3YnnzaaC/fzr4+kNttw97BmMOsE=";
        srcRoot = "tree-sitter-markdown-inline";
      }
      {
        name = "mlir";
        hash = "sha256-osGvK8qxAL1VPdoygibBE6hIn/3zR7pAF/HvmjBZD4M=";
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
    ];
  };
}
