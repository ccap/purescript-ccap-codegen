let
  pkgs = import <nixpkgs> {};

  easy-ps = import (pkgs.fetchFromGitHub {
    owner = "justinwoo";
    repo = "easy-purescript-nix";

    # at this git revision:
    rev = "828e34277dfd77507324d47f3a5e84afa81183c4";

    # this sha can be obtained by using
    # nix-prefetch-git https://github.com/justinwoo/easy-purescript-nix --rev 828e34277dfd77507324d47f3a5e84afa81183c4
    # (without --rev for latest master)
    sha256 = "1ig0ayqdvbww7f2dvd3dflsmgc16r7df7k0iysb2rmy6ykf3balq";
  });
in pkgs.stdenv.mkDerivation {
  name = "purescript-ccap-codegen";
  src = ./.;
  buildInputs = easy-ps.buildInputs;
}
