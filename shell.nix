let 
  lts = "ghc7103";
in 
  with import <nixpkgs> { };
  haskell.lib.buildStackProject {
     ghc = haskell.packages.${lts}.ghc;
     name = "rscoin-core";
     buildInputs = [ zlib glib git cabal-install openssh autoreconfHook stack gmp ];
  }
