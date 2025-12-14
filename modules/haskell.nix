{ pkgs, ... }:

let
  # Pick the GHC version you want
  ghcVersion = "ghc912";

  hs = pkgs.haskell.packages.${ghcVersion};
in {
  environment.systemPackages = [
    hs.ghc
    hs.cabal-install
    hs.haskell-language-server
  ];
}
