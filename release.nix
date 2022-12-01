let pkgs = import ./nixpkgs.nix;
    aoc = pkgs.haskellPackages.callCabal2nix "AoC2022" ./. {};

in pkgs.haskell.lib.overrideCabal aoc (old: {
  enableSharedExecutables = false;
  enableSharedLibraries = false;
  configureFlags = [ ];
})