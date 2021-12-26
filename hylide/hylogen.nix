{ pkgs ? import <nixpkgs> {}, compiler ? "default" }:

let
  src = fetchTarball "https://github.com/sleexyz/hylogen/archive/master.tar.gz";
  oldHaskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler}; 
  haskellPackages = oldHaskellPackages.override {
    overrides = hpNew: hpOld: {
      hylogen = hpNew.callCabal2nix "hylogen" "${src}/hylogen" { };
      hylide = hpNew.callCabal2nix "hylide" "${src}/hylide" { };
    };
  };
  myGhc = haskellPackages.ghcWithPackages (ps: with ps; [ hylogen hylide ]);

  # Wrapper to make hint see the available 
  hylide-invoker = pkgs.stdenv.mkDerivation {
    name = "hylide-invoker";
    unpackPhase = "true";
    buildInputs = [ myGhc pkgs.makeWrapper ];
    buildPhase = ''
      # We need to provide the ghc interpreter (hint) with the location of the ghc lib dir and the package db
      mkdir -p $out/bin
      ln -s ${myGhc}/bin/hylide $out/bin/hylide
      wrapProgram $out/bin/hylide --set GHC_PACKAGE_PATH "${myGhc}/lib/${myGhc.meta.name}/package.conf.d"
      '';
    installPhase = "echo nothing to install";
  };

in

  hylide-invoker
