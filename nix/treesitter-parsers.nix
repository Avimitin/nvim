{ fetchFromGitHub, callPackage, writeTextFile, lib }:
let
  mkTreesitter = callPackage ./treesitter.nix { };
in
{
  # Nix black magic to get OOP-like object method, this will return a list of tree-sitter parser derivation instead of key-value pair.
  #
  # Example:
  #
  # ```nix
  # parsers = pkgs.callPackage ./treesitter-parsers.nix {};
  # drvs = parsers [ "bash" "nix" ]; // => drvs is now a list with only bash and nix treesitter parser derivation.
  # ```
  #
  # @param: langs [ string ] A list to specify what derivations will be return. Empty list means return all.
  __functor = with builtins;
    self: langs: lib.pipe self [
      (lib.filterAttrs (_: lib.isDerivation))
      (lib.filterAttrs (k: _:
        if (length langs == 0) then
          true
        else
          any (wanted: k == wanted) langs))
      lib.attrValues
    ];

  bash = mkTreesitter {
    lang = "bash";
    src = fetchFromGitHub {
      owner = "tree-sitter";
      repo = "tree-sitter-bash";
      rev = "bdcd56c5a3896f7bbb7684e223c43d9f24380351";
      sha256 = "sha256-zkhCk19kd/KiqYTamFxui7KDE9d+P9pLjc1KVTvYPhI=";
    };
    version = "0.20.0+rev=bdcd56c";
  };
  c = mkTreesitter {
    lang = "c";
    src = fetchFromGitHub {
      owner = "tree-sitter";
      repo = "tree-sitter-c";
      rev = "39bea7d391f57c5f0e061419e1c3066e03eb14b3";
      sha256 = "sha256-0iE7dRvouBZuVliWCuuM81CBlPndHR+qFEX8UnOSKWg=";
    };
    version = "v0.20.6";
  };
  cpp = mkTreesitter {
    lang = "cpp";
    src = fetchFromGitHub {
      owner = "tree-sitter";
      repo = "tree-sitter-cpp";
      rev = "a90f170f92d5d70e7c2d4183c146e61ba5f3a457";
      sha256 = "sha256-e9Mz84lssaPR80hlogyjXx+jA8gD8YVp4T06qC6gRVI=";
    };
    version = "0.20.3+rev=a90f170";
  };
  css = mkTreesitter {
    lang = "css";
    src = fetchFromGitHub {
      owner = "tree-sitter";
      repo = "tree-sitter-css";
      rev = "fec7d3757ab8f46a0ffe298be99b16ad5b9fa229";
      sha256 = "sha256-f3+pvJtULuJ6SHcmrMYyvreSAeEsq3L2+5V3dhloaj8=";
    };
    version = "0.0.0+rev=fec7d37";
  };
  diff = mkTreesitter {
    lang = "diff";
    src = fetchFromGitHub {
      owner = "the-mikedavis";
      repo = "tree-sitter-diff";
      rev = "c165725c28e69b36c5799ff0e458713a844f1aaf";
      sha256 = "sha256-qou5ow/Am/qyO0I1j74ojgnBonwmJriLCCeSNpTk7t8=";
    };
    version = "0.0.0+rev=fec7d37";
  };
  firrtl = mkTreesitter {
    lang = "firrtl";
    src = fetchFromGitHub {
      owner = "amaanq";
      repo = "tree-sitter-firrtl";
      rev = "2b5adae629c8cba528c7b1e4aa67a8ae28934ea5";
      sha256 = "sha256-If34GymYMJpSNtzSGpcq4dMxj8djKZ3B5uMHGx9uCnM=";
    };
    version = "0.0.0+rev=2b5adae";
  };
  gitcommit = mkTreesitter {
    lang = "gitcommit";
    src = fetchFromGitHub {
      owner = "gbprod";
      repo = "tree-sitter-gitcommit";
      rev = "6856a5fd0ffeff17d83325a8ce4e57811010eff1";
      sha256 = "sha256-OD+lGLsMRFRPHwnXoM78t95QvjO0OQN4ae3z3wy5DO4=";
    };
    version = "0.0.0+rev=6856a5f";
  };
  llvm = mkTreesitter {
    lang = "llvm";
    src = fetchFromGitHub {
      owner = "benwilliamgraham";
      repo = "tree-sitter-llvm";
      rev = "1b96e58faf558ce057d4dc664b904528aee743cb";
      sha256 = "sha256-9OCiD7Hotl7EYoggX0lArwFvK2OZisBUsX7xv8+Ig+o=";
    };
    version = "0.0.0+rev=1b96e58";
  };
  lua = mkTreesitter {
    lang = "lua";
    src = fetchFromGitHub {
      owner = "MunifTanjim";
      repo = "tree-sitter-lua";
      rev = "9668709211b2e683f27f414454a8b51bf0a6bda1";
      sha256 = "sha256-5t5w8KqbefInNbA12/jpNzmky/uOUhsLjKdEqpl1GEc=";
    };
    version = "0.0.19+rev=9668709";
  };
  markdown = mkTreesitter {
    lang = "markdown";
    src = fetchFromGitHub {
      owner = "MDeiml";
      repo = "tree-sitter-markdown";
      rev = "aaf76797aa8ecd9a5e78e0ec3681941de6c945ee";
      sha256 = "sha256-4HofUc+OsI3d2CN9mex5ROwuRHyB6fGow8gl0fe5es4=";
    };
    version = "0.1.6+rev=aaf7679";
    srcRoot = "tree-sitter-markdown";
  };
  markdown_inline = mkTreesitter {
    lang = "markdown_inline";
    src = fetchFromGitHub {
      owner = "MDeiml";
      repo = "tree-sitter-markdown";
      rev = "aaf76797aa8ecd9a5e78e0ec3681941de6c945ee";
      sha256 = "sha256-4HofUc+OsI3d2CN9mex5ROwuRHyB6fGow8gl0fe5es4=";
    };
    version = "0.1.6+rev=aaf7679";
    srcRoot = "tree-sitter-markdown-inline";
  };
  mlir = mkTreesitter {
    lang = "mlir";
    version = "e2053f7c";
    src = fetchFromGitHub {
      owner = "artagnon";
      repo = "tree-sitter-mlir";
      rev = "e2053f7c8856d91bc36c87604f697784845cee69";
      sha256 = "sha256-u41Qyyu9bNbcAjfTUoq2W2LvfqPpJ62xzaaAg3VbTsA=";
    };
    needs_generate = true;
  };
  ocaml = mkTreesitter {
    lang = "ocaml";
    version = "0.20.4";
    src = fetchFromGitHub {
      owner = "tree-sitter";
      repo = "tree-sitter-ocaml";
      rev = "694c57718fd85d514f8b81176038e7a4cfabcaaf";
      sha256 = "sha256-X4iQ60LgiVsF0rtinVysX16d6yFjaSmwwndP2L5cuqw=";
    };
  };
  regex = mkTreesitter {
    lang = "regex";
    version = "0.0.0+rev=2354482";
    src = fetchFromGitHub {
      owner = "tree-sitter";
      repo = "tree-sitter-regex";
      rev = "2354482d7e2e8f8ff33c1ef6c8aa5690410fbc96";
      sha256 = "sha256-X4iQ60LgiVsF0rtinVysX16d6yFjaSmwwndP2L5cuqw=";
    };
  };
  ruby = mkTreesitter {
    lang = "ruby";
    version = "0.0.0+rev=f257f3f5";
    src = fetchFromGitHub {
      owner = "tree-sitter";
      repo = "tree-sitter-ruby";
      rev = "f257f3f57833d584050336921773738a3fd8ca22";
      sha256 = "sha256-0EaU9O67faGwtO1GIxjK4Uv1etd0p1vtfrVB3d6TDF8=";
    };
  };
  rust = mkTreesitter {
    lang = "rust";
    version = "0.0.0+rev=17a6b15";
    src = fetchFromGitHub {
      owner = "tree-sitter";
      repo = "tree-sitter-rust";
      rev = "17a6b15562b09db1f27b8f5f26f17edbb2aac097";
      sha256 = "sha256-seWoMuA87ZWCq3mUXopVeDCcTxX/Bh+B4PFLVa0CBQA=";
    };
  };
  scala = mkTreesitter {
    lang = "scala";
    version = "0.0.0+rev=70afdd56";
    src = fetchFromGitHub {
      owner = "tree-sitter";
      repo = "tree-sitter-scala";
      rev = "70afdd5632d57dd63a960972ab25945e353a52f6";
      sha256 = "sha256-bi0Lqo/Zs2Uaz1efuKAARpEDg5Hm59oUe7eSXgL1Wow=";
    };
  };
  nix = mkTreesitter {
    lang = "nix";
    src = fetchFromGitHub {
      owner = "nix-community";
      repo = "tree-sitter-nix";
      rev = "66e3e9ce9180ae08fc57372061006ef83f0abde7";
      sha256 = "sha256-+o+f1TlhcrcCB3TNw1RyCjVZ+37e11nL+GWBPo0Mxxg=";
    };
    version = "0.0.0+rev=66e3e9c";
  };
}
