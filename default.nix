{ pkgs ? import <nixpkgs> {} }:
let
  filterSite = path: _:
    let baseName = baseNameOf (toString path); in
    baseName != "design" &&
    baseName != "builder" &&
    baseName != "builder.cabal" &&
    baseName != "site.hs" &&
    baseName != "_cache" &&
    baseName != "_site" &&
    !(pkgs.lib.hasPrefix ".ghc.environment." baseName);

  filterBuilder = path: _:
    baseNameOf path == "builder.cabal" ||
    baseNameOf path == "site.hs";
in rec {
  builder = pkgs.haskellPackages.callCabal2nix "builder"
    (builtins.filterSource filterBuilder ./.) {};

  site = pkgs.stdenv.mkDerivation rec {
    name = "zarybnicky.com";
    src = builtins.filterSource filterSite ./.;
    phases = "unpackPhase buildPhase";
    buildInputs = [ builder ];
    buildPhase = ''
      export LOCALE_ARCHIVE="${pkgs.glibcLocales}/lib/locale/locale-archive";
      export LANG=en_US.UTF-8
      site build
      mkdir -p $out
      cp -r _site/* $out
    '';
  };
}
