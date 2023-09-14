{ lib, stdenv, nodejs, tree-sitter }:


{ lang, src, version, needs_generate ? false, srcRoot ? null }:
stdenv.mkDerivation {
  pname = "tree-sitter-${lang}";
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
    mv parser $out/parser/${lang}.so
    runHook postInstall
  '';
}
