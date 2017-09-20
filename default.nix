{nixpkgs ? import <nixpkgs> { }, ghc ? nixpkgs.ghc}:

with nixpkgs;

haskell.lib.buildStackProject {
  name = "wcVector";
  buildInputs = [ zlib.dev zlib.out pkgconfig llvm_37 ncurses git ];
  inherit ghc;
}