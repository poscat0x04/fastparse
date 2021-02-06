{
  inputs.nixpkgs.url = github:NixOS/nixpkgs/nixos-unstable;
  inputs.flake-utils.url = github:poscat0x04/flake-utils;

  outputs = { self, nixpkgs, flake-utils, ... }: with flake-utils;
    eachDefaultSystem (
      system:
        let
          pkgs = import nixpkgs { inherit system; overlays = [ self.overlay ]; };
        in
          with pkgs;
          {
            devShell = fastparse-dev.envFunc { withHoogle = true; };
            defaultPackage = fastparse;
          }
    ) // {
      overlay = self: super:
        let
          hpkgs = super.haskellPackages;
          fastparse = super.haskell.lib.doBenchmark (hpkgs.callCabal2nix "fastparse" ./. {});
        in
          with super; with haskell.lib;
          {
            inherit fastparse;
            fastparse-dev = addBuildTools fastparse [
              haskell-language-server
              cabal-install
            ];
          };
    };
}
