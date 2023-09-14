{ fetchFromGitHub, fetchurl, callPackage, writeTextFile, lib }:
let
  mkTreesitter = callPackage ./treesitter.nix { };

  nvimTSRev = "v0.9.1";
  nvimTreesitterLockFile = fetchurl {
      name = "nvim-treesitter-parser-lock-${nvimTSRev}.json";
      url = "https://raw.githubusercontent.com/nvim-treesitter/nvim-treesitter/${nvimTSRev}/lockfile.json";
      hash = "sha256-T41dX7aQio+yqGDWBgHhtjLWqRdcFrVCcKgvb8bsYFg=";
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
      rev = nvimTreesitterLock.${lang}.revision;
      shortrev = lib.pipe rev [
          lib.stringToCharacters
          (lib.sublist 0 7)
          lib.concatStrings
        ];
    in
    fetchFromGitHub ({
      repo = "tree-sitter-${lang}";
      inherit rev;
      # Specify source name to trigger rebuild when version changed
      name = "tree-sitter-${lang}-${shortrev}-src";

      passthru = { inherit shortrev; };
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
      sha256 = "sha256-ueZjazaqjbxqCM7mO8h9m0fJ6RUCaX4MuJx7StnPqyc=";

      inherit lang;
    };
    version = "${nvimTSRev}-compatible+rev=${src.passthru.shortrev}";
  };
  c = mkTreesitter rec{
    lang = "c";
    src = fetchTSFromGitHub {
      owner = "tree-sitter";
      sha256 = "sha256-5n9ZnslpUWyusWTo+AqJiPGB64NB0rTbc2rtfByPUC8=";

      inherit lang;
    };
    version = "${nvimTSRev}-compat+rev=${src.passthru.shortrev}";
  };
  cpp = mkTreesitter rec{
    lang = "cpp";
    src = fetchTSFromGitHub {
      owner = "tree-sitter";
      sha256 = "sha256-/w77s0qcJcLH6MX3BVuM37UQ1Xm/6t2oJ2KTq+hnIJI=";

      inherit lang;
    };
    version = "${nvimTSRev}-compat+rev=${src.passthru.shortrev}";
  };
  css = mkTreesitter rec{
    lang = "css";
    src = fetchTSFromGitHub {
      owner = "tree-sitter";
      sha256 = "sha256-HBCxnetErHqhSJeEIHFTaSqt8aJgJ4+OOgw8p+NDVDo=";

      inherit lang;
    };
    version = "${nvimTSRev}-compat+rev=${src.passthru.shortrev}";
  };
  diff = mkTreesitter rec{
    lang = "diff";
    src = fetchTSFromGitHub {
      owner = "the-mikedavis";
      sha256 = "sha256-qou5ow/Am/qyO0I1j74ojgnBonwmJriLCCeSNpTk7t8=";

      inherit lang;
    };
    version = "${nvimTSRev}-compat+rev=${src.passthru.shortrev}";
  };
  firrtl = mkTreesitter rec{
    lang = "firrtl";
    src = fetchTSFromGitHub {
      owner = "amaanq";
      sha256 = "sha256-If34GymYMJpSNtzSGpcq4dMxj8djKZ3B5uMHGx9uCnM=";

      inherit lang;
    };
    version = "${nvimTSRev}-compat+rev=${src.passthru.shortrev}";
  };
  gitcommit = mkTreesitter rec{
    lang = "gitcommit";
    src = fetchTSFromGitHub {
      owner = "gbprod";
      sha256 = "sha256-OD+lGLsMRFRPHwnXoM78t95QvjO0OQN4ae3z3wy5DO4=";

      inherit lang;
    };
    version = "${nvimTSRev}-compat+rev=${src.passthru.shortrev}";
  };
  llvm = mkTreesitter rec{
    lang = "llvm";
    src = fetchTSFromGitHub {
      owner = "benwilliamgraham";
      sha256 = "sha256-CK7f0qSAsec2cuQElXLFRQ5uiQLGCyEpNIKTIDwbBrU=";

      inherit lang;
    };
    version = "${nvimTSRev}-compat+rev=${src.passthru.shortrev}";
  };
  lua = mkTreesitter rec{
    lang = "lua";
    src = fetchTSFromGitHub {
      owner = "MunifTanjim";
      sha256 = "sha256-GrRHbNVKijYNeICeopVW6OtHquqKhKtDDa7dK5sEMNQ=";

      inherit lang;
    };
    version = "${nvimTSRev}-compat+rev=${src.passthru.shortrev}";
  };
  markdown = mkTreesitter rec{
    lang = "markdown";
    src = fetchTSFromGitHub {
      owner = "MDeiml";
      sha256 = "sha256-4HofUc+OsI3d2CN9mex5ROwuRHyB6fGow8gl0fe5es4=";

      inherit lang;
    };
    version = "${nvimTSRev}-compat+rev=${src.passthru.shortrev}";
    srcRoot = "tree-sitter-markdown";
  };
  markdown_inline = mkTreesitter rec{
    lang = "markdown_inline";
    src = fetchTSFromGitHub {
      owner = "MDeiml";
      sha256 = "sha256-4HofUc+OsI3d2CN9mex5ROwuRHyB6fGow8gl0fe5es4=";

      inherit lang;
    };
    version = "${nvimTSRev}-compat+rev=${src.passthru.shortrev}";
    srcRoot = "tree-sitter-markdown-inline";
  };
  mlir = mkTreesitter rec{
    lang = "mlir";
    version = "${nvimTSRev}-compat+rev=${src.passthru.shortrev}";
    src = fetchTSFromGitHub {
      owner = "artagnon";
      sha256 = "sha256-u41Qyyu9bNbcAjfTUoq2W2LvfqPpJ62xzaaAg3VbTsA=";

      inherit lang;
    };
    needs_generate = true;
  };
  ocaml = mkTreesitter rec{
    lang = "ocaml";
    version = "${nvimTSRev}-compat+rev=${src.passthru.shortrev}";
    src = fetchTSFromGitHub {
      owner = "tree-sitter";
      sha256 = "sha256-j3Hv2qOMxeBNOW+WIgIYzG3zMIFWPQpoHe94b2rT+A8=";

      inherit lang;
    };
    srcRoot = "ocaml";
  };
  ocaml_interface = mkTreesitter rec{
    lang = "ocaml_interface";
    version = "${nvimTSRev}-compat+rev=${src.passthru.shortrev}";
    src = fetchTSFromGitHub {
      owner = "tree-sitter";
      sha256 = "sha256-j3Hv2qOMxeBNOW+WIgIYzG3zMIFWPQpoHe94b2rT+A8=";

      inherit lang;
    };
    srcRoot = "interface";
  };
  regex = mkTreesitter rec{
    lang = "regex";
    version = "${nvimTSRev}-compat+rev=${src.passthru.shortrev}";
    src = fetchTSFromGitHub {
      owner = "tree-sitter";
      sha256 = "sha256-X4iQ60LgiVsF0rtinVysX16d6yFjaSmwwndP2L5cuqw=";

      inherit lang;
    };
  };
  ruby = mkTreesitter rec{
    lang = "ruby";
    version = "${nvimTSRev}-compat+rev=${src.passthru.shortrev}";
    src = fetchTSFromGitHub {
      owner = "tree-sitter";
      sha256 = "sha256-0EaU9O67faGwtO1GIxjK4Uv1etd0p1vtfrVB3d6TDF8=";

      inherit lang;
    };
  };
  rust = mkTreesitter rec{
    lang = "rust";
    version = "${nvimTSRev}-compat+rev=${src.passthru.shortrev}";
    src = fetchTSFromGitHub {
      owner = "tree-sitter";
      sha256 = "sha256-CrNY+4nsYQOzzVR7X+yuo4+5s6K3VHtVQyWfledKJ1U=";

      inherit lang;
    };
  };
  scala = mkTreesitter rec{
    lang = "scala";
    version = "${nvimTSRev}-compat+rev=${src.passthru.shortrev}";
    src = fetchTSFromGitHub {
      owner = "tree-sitter";
      sha256 = "sha256-SRj4iF1qS2jEFaIkRfXzAmzG7jKeSzKv5/GdXKbKRjU";

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
    version = "${nvimTSRev}-compat+rev=${src.passthru.shortrev}";
  };

  passthru = {
    treesitter-lock-file = nvimTreesitterLockFile;
  };
}
