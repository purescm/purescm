{
  description = "Chez Scheme backend for PureScript";

  inputs = {
    # we are stuck on 23.05 because clang is broken on macos-x86 until macos 15:
    # https://github.com/NixOS/nixpkgs/issues/234710
    nixpkgs.url = "github:nixos/nixpkgs/release-23.05";

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
            chez = pkgs.chez-racket.overrideAttrs (final: prev: {
              postFixup = if pkgs.stdenv.isDarwin then ''
                install_name_tool -add_rpath ${pkgs.pcre2.out}/lib $out/bin/scheme
              ''
              else ''
                patchelf $out/bin/scheme --add-rpath ${pkgs.pcre2.out}/lib
              '';
            });
        in {
          default = pkgs.mkShell {
            name = "purescm";
            packages = with pkgs; [
              purescript-language-server
              purs-backend-es
              purs-bin.purs-0_15_10
              purs-tidy
              spago-unstable
              nodejs-slim
              esbuild
              chez
              pkg-config
            ];
          };
        });

      formatter = forAllSystems (system: nixpkgsFor.${system}.nixpkgs-fmt);
    };
}
