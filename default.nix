{nixpkgs ? import <nixpkgs> { }, ghc ? nixpkgs.ghc}:

with nixpkgs;

haskell.lib.buildStackProject {
  name = "wcVector";
  buildInputs = [ zlib.dev zlib.out pkgconfig ncurses git ];
  ghc = haskell.compiler.ghc802;
 }