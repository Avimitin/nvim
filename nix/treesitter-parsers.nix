{ fetchurl, callPackage, lib }:
let
  treesitterRevision = "f3fb301b267e85e4cbc725561da4a82b1c3089c8";

  nvimTreesitterParserInfoFile = callPackage ./nvim-treesitter-parsers-info.nix { expect-version = treesitterRevision; };
  # Get parser information from nvim-treesitter's lockfile.json
  parsers-info = lib.importJSON "${nvimTreesitterParserInfoFile}";

  # Use the information from nvim-treesitter to fetch the source.
  # This function will inject the source and version information to the pass-in parameter and eliminate the `hash` attribute.
  # For example: { name = ..; hash = ...; srcRoot = ...; } => { name = ...; srcRoot = ...; src = ...; version = ...; }
  convertInfoToParserSrc = { name, hash, ... } @ input:
    assert lib.assertMsg
      (parsers-info ? "${name}")
      "Language '${name}' is not supported by nvim-treesitter ${treesitterRevision} yet";

    let
      url = parsers-info.${name}.url;
      rev = parsers-info.${name}.revision;
      shortrev = builtins.substring 0 7 rev;
      override = {
        src = fetchurl {
          name = "tree-sitter-${name}-${shortrev}-src.tar.gz";
          url = "${url}/archive/${rev}.tar.gz";

          inherit hash;
        };
        version = "${treesitterRevision}-compat+rev=${shortrev}";
      };
    in
    # Remove the hash attr in case some unexpected things happen
    removeAttrs (input // override) [ "hash" ];

  # Convert the name and hash attrsets to derivation sets.
  #
  # Expect the argument in this form: [ { name: xxx; hash: xxx; }, { ... } ],
  #   where:
  #     - name string: The name of the language
  #     - hash string: The input hash, you can leave it blank and wait for nix hash report the correct hash
  #     - needs_generate bool: When true, tree-sitter CLI will be used to generate the parser.
  #     - srcRoot string: Specify where the parser source located. Some repository will vendor two or more parser source code in one repository.
  #
  # The input list will be finally generated into a attrset where key is the language name, and value is the parser derivation.
  #  [ { name: xxx; hash: xxx; }, { ... } ] => { nameA: <derivationA>; nameB: <derivationB>; }
  #
  # The return attrset support filtering by calling itself with a list argument.
  # For example, if the return attrset have three language parser, "bash", "lua" and "rust":
  #
  # ```nix
  # parsers.bash # => derivation
  # parsers.lua # => derivation
  #
  # let filteredParsers = parsers [ "rust" ] in
  #   parsers.rust # => derivation
  #   parsers ? "bash" # => false
  # ```
  #
  # NOTICE: The returning value is a really hacky nix expression. It is a set when you use `.` to get its member,
  # but it will be a function when you give or not given parameter to it.
  parserGen = with builtins; langSpecList:
    assert lib.assertMsg (typeOf langSpecList == "list" && length langSpecList > 0) "Expect a list";
    assert lib.assertMsg (all (lang: lang ? "name" && lang ? "hash") langSpecList) "Each item in list must have attr name and hash";

    let
      mkTreesitter = callPackage ./treesitter.nix { };
      parserSet = lib.pipe langSpecList [
        # First, convert those JSON informantion into treesitter source code
        (map convertInfoToParserSrc)
        # Then, convert them into list of key-value pair, where key is the name of the plugin, and value is the plugin derivation.
        (map (lang: lib.nameValuePair lang.name (mkTreesitter lang)))
        # Finally, construct them into attr-set, like { bash = derivation; zig = derivation; };
        builtins.listToAttrs
      ];
    in
    parserSet // {
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
    };
in
parserGen [
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
]
