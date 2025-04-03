{
  description = "Flakes for running this configuration";

  outputs =
    _:
    let
      jsonToSrc =
        file:
        with builtins;
        let
          srcDefines = fromJSON (readFile file);
        in
        mapAttrs (
          name: value:
          fetchTarball {
            inherit (value.src) url sha256;
          }
        ) srcDefines;
      inputs = jsonToSrc ./flake-lock/generated.json;
      overlay = import ./overlay.nix;
    in
    {
      overlays.default = overlay;
      neovimConfig =
        let
          lib = import "${inputs.nixpkgs}/lib";
        in
        with lib.fileset;
        toSource {
          root = ./.;
          fileset = unions [
            ./after
            ./ftdetect
            ./indent
            ./lua
            ./syntax
            ./vsnip
            ./init.lua
            ./lazy-lock.json
          ];
        };
    }
    // (import inputs.flake-utils).eachDefaultSystem (
      system:
      let
        pkgs = import inputs.nixpkgs {
          overlays = [ overlay ];
          inherit system;
        };
        treefmtEval = (import inputs.treefmt-nix).lib.evalModule pkgs {
          projectRootFile = "flake.nix";
          settings.verbose = 1;
          programs.nixfmt.enable = true;
        };
      in
      {
        formatter = treefmtEval.config.build.wrapper;
        legacyPackages = pkgs;
        devShells.default = pkgs.mkShell {
          nativeBuildInputs = with pkgs.haskellPackages; [
            pkgs.ghc-for-ts-plugins
            fourmolu
            haskell-language-server
          ];
        };
      }
    );
}
