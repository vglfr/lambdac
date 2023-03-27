{
  pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/724bfc0892363087709bd3a5a1666296759154b1.tar.gz") {}
}:

pkgs.mkShell {
  buildInputs = [
    pkgs.cabal-install
    pkgs.haskell.compiler.ghc924
  ];
}
