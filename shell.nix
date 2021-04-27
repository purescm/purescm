#!/usr/bin/env nix-shell

{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  name="purescm";
  buildInputs = [ pkgs.stack ];
  shellHook = ''
    alias stack='stack --nix'
  '';
}
