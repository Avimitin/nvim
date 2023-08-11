final: prev:
let
  fennel-ls = final.callPackage
    (
      { stdenv, luajit, fetchFromSourcehut }:
      stdenv.mkDerivation {
        pname = "fennel-ls";
        version = "unstable-2023-07-20";
        propagatedBuildInputs = [ luajit ];
        src = fetchFromSourcehut {
          owner = "~xerool";
          repo = "fennel-ls";
          rev = "e7c642e12a15c6d452559414ee1890b30f4e8406";
          sha256 = "sha256-xPTgGmeILJSNsC++r9x+ussS7ttG7a5Cgl/D02auTgE=";
        };

        makeFlags = [
          "PREFIX=$(out)"
        ];
      }
    )
    { };

in
{
  inherit fennel-ls;
}
