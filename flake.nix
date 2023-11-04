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

    purescript-overlay.url = "github:thomashoneyman/purescript-overlay";
    purescript-overlay.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, flake-utils, purescript-overlay, ... }:
    let supportedSystems = [ "x86_64-linux" "x86_64-darwin" "aarch64-darwin" ];
    in flake-utils.lib.eachSystem supportedSystems (system:
      let
        overlays = [ purescript-overlay.overlays.default ];
        pkgs = import nixpkgs {
          inherit system overlays;
        };
      in
      {
        devShells = {
          default = pkgs.mkShell {
            name = "purescm";
            packages = with pkgs; [
              purescript-language-server
              purs-backend-es
              purs-bin.purs-0_15_10
              purs-tidy
              spago-unstable
              nodejs-slim-16_x
              chez-racket
              esbuild
            ];
          };
        };
        formatter = pkgs.nixpkgs-fmt;
      });
}
