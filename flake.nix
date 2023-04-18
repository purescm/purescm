{
  description = "Chez Scheme backend for PureScript";

  inputs = {
    nixpkgs.url =
      "github:nixos/nixpkgs?rev=402cc3633cc60dfc50378197305c984518b30773";

    flake-utils.url = "github:numtide/flake-utils";

    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };

    easy-purescript-nix = {
      url = "github:f-f/easy-purescript-nix";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, easy-purescript-nix, ... }:
    let supportedSystems = [ "x86_64-linux" "x86_64-darwin" ];
    in flake-utils.lib.eachSystem supportedSystems (system:
      let
        pkgs = import nixpkgs { inherit system; };
        easy-ps = pkgs.callPackage easy-purescript-nix { };
      in
      {
        devShells = {
          default = pkgs.mkShell {
            name = "purescm";
            packages = [
              easy-ps.purs-0_15_8
              easy-ps.purs-tidy
              easy-ps.psa
              easy-ps.spago-next
              easy-ps.purescript-language-server
              pkgs.nodejs-16_x
              pkgs.chez-racket
            ];
          };
        };
        formatter = pkgs.nixpkgs-fmt;
      });
}
