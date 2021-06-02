#!/usr/bin/env nix-shell

# Taken from:
# https://docs.haskellstack.org/en/stable/nix_integration/#using-a-custom-shellnix-file

{ pkgs ? import <nixpkgs> {}
, ghc }:

pkgs.haskell.lib.buildStackProject {
  inherit ghc;
  name = "purescm-stack";
  buildInputs = [ pkgs.zlib pkgs.spago pkgs.git ];
}
