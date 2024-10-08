{
  description = "Chez Scheme backend for PureScript";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";

    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };

    purescript-overlay.url = "github:thomashoneyman/purescript-overlay";
    purescript-overlay.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, ... }@inputs:
    let
      supportedSystems = [ "x86_64-linux" "x86_64-darwin" "aarch64-darwin" ];

      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;

      nixpkgsFor = forAllSystems (system: import nixpkgs {
        inherit system;
        overlays = builtins.attrValues self.overlays;
      });
    in
    {
      overlays = {
        purescript = inputs.purescript-overlay.overlays.default;
      };

      devShells = forAllSystems (system:
        let pkgs = nixpkgsFor.${system};
        in {
          default = pkgs.mkShell {
            name = "purescm";
            packages = [
              pkgs.purescript-language-server
              pkgs.purs-backend-es
              pkgs.purs-bin.purs-0_15_10
              pkgs.purs-tidy
              pkgs.spago-unstable
              pkgs.nodejs-slim
              pkgs.esbuild
              pkgs.pkg-config
              pkgs.icu
              pkgs.chez
            ];

            CHEZ_DYLD_LIBRARY_PATH="${pkgs.pcre2.out}/lib:${pkgs.icu.out}/lib";
            LD_LIBRARY_PATH = "${pkgs.pcre2.out}/lib:${pkgs.icu.out}/lib";
          };
        });

      formatter = forAllSystems (system: nixpkgsFor.${system}.nixpkgs-fmt);
    };
}
