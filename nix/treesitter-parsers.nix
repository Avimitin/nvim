{ fetchFromGitHub, fetchurl, callPackage, writeTextFile, lib }:
let
  mkTreesitter = callPackage ./treesitter.nix { };

  nvimTreesitterLockFile =
    let
      rev = "9ab4e9cc8989e3811b14897cd0eb21ae35e5541e";
    in
    fetchurl {
      name = "nvim-treesitter-parser-lock-${rev}.json";
      url = "https://raw.githubusercontent.com/nvim-treesitter/nvim-treesitter/${rev}/lockfile.json";
      hash = "sha256-woiNNmu7MnAnqbaGmZITy9kkawPr9CRYtdQMjeEbAmA=";
    };
  nvimTreesitterLock = lib.importJSON "${nvimTreesitterLockFile}";

  # Needs a { owner = ?; lang = ?; hash = ? } spec
  fetchTSFromGitHub = spec:
    # Assertions
    assert lib.assertMsg (spec ? "lang") "No `lang` field";
    assert lib.assertMsg
      (lib.hasAttrByPath [ "${spec.lang}" "revision" ] nvimTreesitterLock)
      "nvim-treesitter doesn't support language ${spec.lang}";

    let
      src_rename = {
        markdown_inline = "markdown";
        ocaml_interface = "ocaml";
      };

      # markdown_inline is vendored in tree-sitter-markdown repo
      lang = if src_rename ? "${spec.lang}" then src_rename.${spec.lang} else spec.lang;
    in
    fetchFromGitHub (rec {
      repo = "tree-sitter-${lang}";
      rev = nvimTreesitterLock.${lang}.revision;
      # Specify source name to trigger rebuild when version changed
      name = "tree-sitter-${lang}-${rev}-src";
    }
    // (removeAttrs spec [ "lang" "repo" "rev" ]));
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

  bash = mkTreesitter rec {
    lang = "bash";
    src = fetchTSFromGitHub {
      owner = "tree-sitter";
      sha256 = "sha256-zkhCk19kd/KiqYTamFxui7KDE9d+P9pLjc1KVTvYPhI=";

      inherit lang;
    };
    version = "0.20.0+rev=bdcd56c";
  };
  c = mkTreesitter rec{
    lang = "c";
    src = fetchTSFromGitHub {
      owner = "tree-sitter";
      sha256 = "sha256-39i06oXMQemfq3Y4TTXai6HFXvURVOif1v2i9LP4sAI=";

      inherit lang;
    };
    version = "v0.20.6";
  };
  cpp = mkTreesitter rec{
    lang = "cpp";
    src = fetchTSFromGitHub {
      owner = "tree-sitter";
      sha256 = "sha256-e9Mz84lssaPR80hlogyjXx+jA8gD8YVp4T06qC6gRVI=";

      inherit lang;
    };
    version = "0.20.3+rev=a90f170";
  };
  css = mkTreesitter rec{
    lang = "css";
    src = fetchTSFromGitHub {
      owner = "tree-sitter";
      sha256 = "sha256-f3+pvJtULuJ6SHcmrMYyvreSAeEsq3L2+5V3dhloaj8=";

      inherit lang;
    };
    version = "0.0.0+rev=fec7d37";
  };
  diff = mkTreesitter rec{
    lang = "diff";
    src = fetchTSFromGitHub {
      owner = "the-mikedavis";
      sha256 = "sha256-qou5ow/Am/qyO0I1j74ojgnBonwmJriLCCeSNpTk7t8=";

      inherit lang;
    };
    version = "0.0.0+rev=fec7d37";
  };
  firrtl = mkTreesitter rec{
    lang = "firrtl";
    src = fetchTSFromGitHub {
      owner = "amaanq";
      sha256 = "sha256-If34GymYMJpSNtzSGpcq4dMxj8djKZ3B5uMHGx9uCnM=";

      inherit lang;
    };
    version = "0.0.0+rev=2b5adae";
  };
  gitcommit = mkTreesitter rec{
    lang = "gitcommit";
    src = fetchTSFromGitHub {
      owner = "gbprod";
      sha256 = "sha256-OD+lGLsMRFRPHwnXoM78t95QvjO0OQN4ae3z3wy5DO4=";

      inherit lang;
    };
    version = "0.0.0+rev=6856a5f";
  };
  llvm = mkTreesitter rec{
    lang = "llvm";
    src = fetchTSFromGitHub {
      owner = "benwilliamgraham";
      sha256 = "sha256-9OCiD7Hotl7EYoggX0lArwFvK2OZisBUsX7xv8+Ig+o=";

      inherit lang;
    };
    version = "0.0.0+rev=1b96e58";
  };
  lua = mkTreesitter rec{
    lang = "lua";
    src = fetchTSFromGitHub {
      owner = "MunifTanjim";
      sha256 = "sha256-5t5w8KqbefInNbA12/jpNzmky/uOUhsLjKdEqpl1GEc=";

      inherit lang;
    };
    version = "0.0.19+rev=9668709";
  };
  markdown = mkTreesitter rec{
    lang = "markdown";
    src = fetchTSFromGitHub {
      owner = "MDeiml";
      sha256 = "sha256-4HofUc+OsI3d2CN9mex5ROwuRHyB6fGow8gl0fe5es4=";

      inherit lang;
    };
    version = "0.1.6+rev=aaf7679";
    srcRoot = "tree-sitter-markdown";
  };
  markdown_inline = mkTreesitter rec{
    lang = "markdown_inline";
    src = fetchTSFromGitHub {
      owner = "MDeiml";
      sha256 = "sha256-4HofUc+OsI3d2CN9mex5ROwuRHyB6fGow8gl0fe5es4=";

      inherit lang;
    };
    version = "0.1.6+rev=aaf7679";
    srcRoot = "tree-sitter-markdown-inline";
  };
  mlir = mkTreesitter rec{
    lang = "mlir";
    version = "e2053f7c";
    src = fetchTSFromGitHub {
      owner = "artagnon";
      sha256 = "sha256-u41Qyyu9bNbcAjfTUoq2W2LvfqPpJ62xzaaAg3VbTsA=";

      inherit lang;
    };
    needs_generate = true;
  };
  ocaml = mkTreesitter rec{
    lang = "ocaml";
    version = "0.20.4";
    src = fetchTSFromGitHub {
      owner = "tree-sitter";
      sha256 = "sha256-j3Hv2qOMxeBNOW+WIgIYzG3zMIFWPQpoHe94b2rT+A8=";

      inherit lang;
    };
    srcRoot = "ocaml";
  };
  ocaml_interface = mkTreesitter rec{
    lang = "ocaml_interface";
    version = "0.20.4";
    src = fetchTSFromGitHub {
      owner = "tree-sitter";
      sha256 = "sha256-j3Hv2qOMxeBNOW+WIgIYzG3zMIFWPQpoHe94b2rT+A8=";

      inherit lang;
    };
    srcRoot = "interface";
  };
  regex = mkTreesitter rec{
    lang = "regex";
    version = "0.0.0+rev=2354482";
    src = fetchTSFromGitHub {
      owner = "tree-sitter";
      sha256 = "sha256-X4iQ60LgiVsF0rtinVysX16d6yFjaSmwwndP2L5cuqw=";

      inherit lang;
    };
  };
  ruby = mkTreesitter rec{
    lang = "ruby";
    version = "0.0.0+rev=f257f3f5";
    src = fetchTSFromGitHub {
      owner = "tree-sitter";
      sha256 = "sha256-0EaU9O67faGwtO1GIxjK4Uv1etd0p1vtfrVB3d6TDF8=";

      inherit lang;
    };
  };
  rust = mkTreesitter rec{
    lang = "rust";
    version = "0.0.0+rev=17a6b15";
    src = fetchTSFromGitHub {
      owner = "tree-sitter";
      sha256 = "sha256-seWoMuA87ZWCq3mUXopVeDCcTxX/Bh+B4PFLVa0CBQA=";

      inherit lang;
    };
  };
  scala = mkTreesitter rec{
    lang = "scala";
    version = "0.0.0+rev=70afdd56";
    src = fetchTSFromGitHub {
      owner = "tree-sitter";
      sha256 = "sha256-bi0Lqo/Zs2Uaz1efuKAARpEDg5Hm59oUe7eSXgL1Wow=";

      inherit lang;
    };
  };
  nix = mkTreesitter rec{
    lang = "nix";
    src = fetchTSFromGitHub {
      owner = "nix-community";
      sha256 = "sha256-+o+f1TlhcrcCB3TNw1RyCjVZ+37e11nL+GWBPo0Mxxg=";

      inherit lang;
    };
    version = "0.0.0+rev=66e3e9c";
  };

  passthru = {
    treesitter-lock-file = nvimTreesitterLockFile;
  };
}
