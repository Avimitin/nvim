{ lib, stdenv, nodejs, tree-sitter, fetchNvimTsLockFile, fetchFromGitHub, runCommand, fetchurl, neovim }:

{ name, hash, needs_generate ? false, srcRoot ? null, tsRev ? "49f1b9a7efc794be143f7ddcd60ce18e8164a7f8" }:
let
  # Get parser information from nvim-treesitter's lockfile.json
  parsers-info = lib.importJSON "${fetchNvimTsLockFile tsRev}";

  url = parsers-info.${name}.url;
  rev = parsers-info.${name}.revision;
  shortrev = builtins.substring 0 7 rev;
  src = fetchurl {
    name = "tree-sitter-${name}-${shortrev}-src.tar.gz";
    url = "${url}/archive/${rev}.tar.gz";

    inherit hash;
  };
  version = "unstable-${shortrev}";
in
stdenv.mkDerivation {
  pname = "tree-sitter-${name}";
  inherit src version;

  nativeBuildInputs = lib.optionals needs_generate [ nodejs tree-sitter ];

  CFLAGS = [ "-Isrc" "-O2" ];
  CXXFLAGS = [ "-Isrc" "-O2" ];

  stripDebugList = [ "parser" ];

  configurePhase = lib.optionalString (srcRoot != null) ''
    cd ${srcRoot}
  '' + lib.optionalString needs_generate ''
    tree-sitter generate
  '';

  buildPhase = ''
    runHook preBuild
    if [[ -e src/scanner.cc ]]; then
      $CXX -fPIC -c src/scanner.cc -o scanner.o $CXXFLAGS
    elif [[ -e src/scanner.c ]]; then
      $CC -fPIC -c src/scanner.c -o scanner.o $CFLAGS
    fi
    $CC -fPIC -c src/parser.c -o parser.o $CFLAGS
    rm -rf parser
    $CXX -shared -o parser *.o
    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall
    mkdir -p $out/parser
    mv parser $out/parser/${name}.so
    runHook postInstall
  '';
}
