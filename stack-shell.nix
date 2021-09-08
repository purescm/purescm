#!/usr/bin/env nix-shell

# Taken from:
# https://docs.haskellstack.org/en/stable/nix_integration/#using-a-custom-shellnix-file

{ pkgs ? import <nixpkgs> {}
, ghc }:

let
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

pkgs.haskell.lib.buildStackProject {
  inherit ghc;
  name = "purescm-stack";
  buildInputs = [
    pkgs.zlib
    pkgs.git
    easy-ps.purs-0_14_4
    easy-ps.spago
    pkgs.chez
  ];
}
