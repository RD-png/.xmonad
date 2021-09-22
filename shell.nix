{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc8104" }:
let
  inherit (nixpkgs) pkgs;
  ghc = pkgs.haskell.packages.${compiler}.ghcWithPackages (ps: with ps; [
    xmonad
    xmonad-contrib
    xmonad-extras
    haskell-language-server
  ]);
in
  pkgs.stdenv.mkDerivation {
    name = "my-haskell-env-0";
    buildInputs = [ ghc ];
    shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
  }
