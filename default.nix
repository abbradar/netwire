# -*-conf-*-

{ }:
with import <nixpkgs> { };

let
  hs = haskellPackages;
  inherit (hs) cabal;
in rec {

  netwire =
    cabal.mkDerivation (self : rec {
      pname = "netwire";
      version = "5.0.0";
      isLibrary = true;
      isExecutable = false;

      src = ./dist/netwire- + "${version}.tar.gz";

      buildDepends = [
        hs.parallel
        hs.profunctors
        hs.QuickCheck
        hs.semigroups
        hs.testFramework
        hs.testFrameworkQuickcheck2
        hs.testFrameworkTh
      ];
    });

}
