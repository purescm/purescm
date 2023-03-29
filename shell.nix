let
  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/402cc3633cc60dfc50378197305c984518b30773.tar.gz";
    sha256 = "0yvlprdkqg1kizg83j7nivlc58zk7llrbf82jqvgjimrwhsva1m9";
  };

  pkgs = import nixpkgs {};

  easy-ps = import
    # On f-f's fork of easy-purescript-nix, as spago-next is not in mainline yet
    (pkgs.fetchFromGitHub {
      owner = "f-f";
      repo = "easy-purescript-nix";
      rev = "b02cff3db1671fc8fb76a680597a216a9c9b2d03";
      sha256 = "sha256-vfzrVwBntXao3nMi2hkjYlWGUnuyUOVmYi95mQQ0EEY=";
    }) {
    inherit pkgs;
  };
in
pkgs.mkShell {
  buildInputs = [
    easy-ps.purs-0_15_8
    easy-ps.purs-tidy
    easy-ps.psa
    easy-ps.spago-next
    pkgs.nodejs-slim-16_x
    pkgs.chez-racket
  ];
}
