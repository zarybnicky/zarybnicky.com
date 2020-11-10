{
  inputs.hakyll = { url = github:jaspervdj/hakyll/master; flake = false; };

  outputs = { self, nixpkgs, hakyll }: let
    pkgs = import nixpkgs { system = "x86_64-linux"; };

    filterSite = path: _:
      let baseName = baseNameOf (toString path); in
      baseName != "design" &&
      baseName != "builder" &&
      baseName != "builder.cabal" &&
      baseName != "site.hs" &&
      baseName != "_cache" &&
      baseName != "_site" &&
      !(nixpkgs.lib.hasPrefix ".ghc.environment." baseName);

    filterBuilder = path: _:
      baseNameOf path == "builder.cabal" ||
      baseNameOf path == "site.hs";

    hsPackages = pkgs.haskellPackages.extend (pkgs.haskell.lib.packageSourceOverrides {
      inherit hakyll;
      zarybnicky-com-builder = builtins.filterSource filterBuilder ./.;
    });

    zarybnicky-com = pkgs.stdenv.mkDerivation rec {
      name = "zarybnicky.com";
      src = builtins.filterSource filterSite ./.;
      phases = "unpackPhase buildPhase";
      buildInputs = [ hsPackages.zarybnicky-com-builder ];
      buildPhase = ''
        export LOCALE_ARCHIVE="${pkgs.glibcLocales}/lib/locale/locale-archive";
        export LANG=en_US.UTF-8
        site build
        mkdir -p $out
        cp -r _site/* $out
      '';
    };
  in {
    defaultPackage.x86_64-linux = zarybnicky-com;
    packages.x86_64-linux = {
      inherit (hsPackages) zarybnicky-com-builder;
      inherit zarybnicky-com;
    };
    devShell.x86_64-linux = hsPackages.shellFor {
      packages = p: [ p.zarybnicky-com-builder ];
    };
    nixosModule = { config, lib, pkgs, ... }: let
      pkgName = "zarybnicky-com";
      cfg = config.services.${pkgName};
    in {
      options.services.${pkgName} = {
        enable = lib.mkEnableOption "${pkgName}";
        domain = lib.mkOption {
          type = lib.types.str;
          description = "${pkgName} Nginx vhost domain";
          example = "zarybnicky.com";
        };
        stateDir = lib.mkOption {
          type = lib.types.str;
          description = "${pkgName} state directory";
          example = "/var/www/zarybnicky.com";
        };
      };
      config = lib.mkIf cfg.enable {
        services.nginx = {
          enable = true;
          enableReload = true;
          recommendedGzipSettings = true;
          recommendedOptimisation = true;
          recommendedProxySettings = true;

          virtualHosts.${cfg.domain}.locations = let
            pkg = self.packages.x86_64-linux.zarybnicky-com;
          in {
            "/".root = pkg;
            "/".extraConfig = ''
              etag off;
              add_header Last-Modified "";
              add_header etag W/"${builtins.substring 11 32 "${pkg}"}";
            '';
            "/static".root = "/var/www/zarybnicky.com";
          };
        };
      };
    };
  };
}
