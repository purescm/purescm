#!/usr/bin/env nix-shell

let
  pkgs = import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/21.05.tar.gz";
  }) {};

  easy-ps = import
    (pkgs.fetchFromGitHub {
      owner = "justinwoo";
      repo = "easy-purescript-nix";
      rev = "5716cd791c999b3246b4fe173276b42c50afdd8d";
      sha256 = "1r9lx4xhr42znmwb2x2pzah920klbjbjcivp2f0pnka7djvd2adq";
    }) {
    inherit pkgs;
  };
in
pkgs.mkShell {
  name="purescm";
  buildInputs = [
    pkgs.stack
    easy-ps.purs-0_14_4
    easy-ps.spago
    pkgs.python39
    pkgs.chez
  ];
  shellHook = ''
    alias stack='stack --nix'
  '';
}
