{
  description = "canica's description";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/2df15ba83d0510a56f2583fd3481723835acb5a1";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };
  outputs = inputs@{ self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" "aarch64-darwin" ] (system:
      let
        overlays = [ ];
        pkgs =
          import nixpkgs { inherit system overlays; config.allowBroken = true; };
        haskellPackages = pkgs.haskellPackages.override {
          overrides = self: super:
            # https://github.com/NixOS/nixpkgs/issues/140774#issuecomment-976899227
            let
              workaround140774 = hpkg: with pkgs.haskell.lib;
                overrideCabal hpkg (drv: {
                  enableSeparateBinOutput = false;
                });
            in
            {
              ghcid = workaround140774 super.ghcid;
              ormolu = workaround140774 super.ormolu;
              hylide = self.callPackage ./hylide { };
              hylogen = self.callPackage ./hylogen { };
              fsnotify = pkgs.haskell.lib.dontCheck (haskellPackages.callHackage "fsnotify" "0.2.1.2" { });
            };
        };
        project = returnShellEnv:
          haskellPackages.developPackage {
            inherit returnShellEnv;
            name = "canica";
            root = ./.;
            withHoogle = false;
            overrides = self: super:
              # Use callCabal2nix to override Haskell dependencies here
              # cf. https://tek.brick.do/K3VXJd8mEKO7
              let
                workaround140774 = hpkg: with pkgs.haskell.lib;
                  overrideCabal hpkg (drv: {
                    enableSeparateBinOutput = false;
                  });
              in
              {
                ghcid = workaround140774 super.ghcid;
                ormolu = workaround140774 super.ormolu;
              };
            modifier = drv:
              pkgs.haskell.lib.addBuildTools drv
                (with haskellPackages; [
                  # Specify your build/dev dependencies here. 
                  cabal-install
                  cabal-fmt
                  ghcid
                  ormolu
                  haskell-language-server
                  pkgs.nixpkgs-fmt
                  hylide
                  hylogen
                  cabal2nix
                  reflex-dom-core
                  reflex
                  ghc-prim
                  ghcjs-dom
                  jsaddle
                  jsaddle-warp
                  clay
                ]);
          };
      in
      {
        # Used by `nix build` & `nix run` (prod exe)
        defaultPackage = project false;

        # Used by `nix develop` (dev shell)
        devShell = project true;
      });
}
