{
  outputs = { self, nixpkgs }: let
    inherit (builtins) filterSource;
    inherit (nixpkgs.lib) flip;
    inherit (pkgs.nix-gitignore) gitignoreSourcePure;

    pkgs = import nixpkgs {
      system = "x86_64-linux";
      overlays = [ self.overlay ];
    };
    src = gitignoreSourcePure [./.gitignore] ./.;

  in {
    overlay = final: prev: {
      haskell = prev.haskell // {
        packageOverrides = prev.lib.composeExtensions (prev.haskell.packageOverrides or (_: _: {})) (hself: hsuper: {
          builder = prev.haskell.lib.justStaticExecutables (
            hself.callCabal2nix "builder" src {}
          );
        });
      };

      gatsby-builder-modules = final.callPackage ./gatsby/modules.nix {};

      zarybnicky-com-builder = final.haskellPackages.builder;
      zarybnicky-com = final.stdenv.mkDerivation {
        name = "zarybnicky.com";
        src = src;
        phases = "unpackPhase buildPhase";
        buildInputs = [ final.zarybnicky-com-builder ];
        buildPhase = ''
          export LOCALE_ARCHIVE="${pkgs.glibcLocales}/lib/locale/locale-archive";
          export LANG=en_US.UTF-8
          site build
          mkdir -p $out
          cp -r _site/* $out
        '';
      };
    };

    defaultPackage.x86_64-linux = self.packages.x86_64-linux.zarybnicky-com;
    packages.x86_64-linux = {
      inherit (pkgs) zarybnicky-com-builder zarybnicky-com gatsby-builder-modules;
    };
    devShell.x86_64-linux = pkgs.haskellPackages.shellFor {
      packages = p: [ p.builder ];
      buildInputs = [pkgs.nodePackages.gatsby-cli];
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
        vaultDir = lib.mkOption {
          type = lib.types.str;
          description = "${pkgName} vault source";
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

          virtualHosts.${cfg.domain} = {
            enableACME = true;
            forceSSL = true;
            serverAliases = [ "www.${cfg.domain}" ];
            locations = {
              "/".root = pkgs.zarybnicky-com;
              "/".extraConfig = ''
                etag off;
                add_header Last-Modified "";
                add_header etag W/"${builtins.substring 11 32 "${pkgs.zarybnicky-com}"}";
              '';
              "/static".root = "/var/www/zarybnicky.com";
              "/gatsby/".alias = pkgs.callPackage ./gatsby/builder.nix {
                vault = cfg.vaultDir;
              } + "/";
            };
          };
        };
      };
    };
  };
}
