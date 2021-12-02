let pkgs = import (fetchTarball path) {};
    path = https://github.com/NixOS/nixpkgs/archive/master.tar.gz;
in with pkgs;
  mkShell {
    buildInputs = [
      cabal-install
      haskell.compiler.ghc901
      llvmPackages_12.llvm
    ];
  }
