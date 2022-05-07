{
  inputs.nixpkgs.url = github:NixOS/nixpkgs/master;

  outputs = { self, nixpkgs }: let
    pkgs = import nixpkgs {
      system = "x86_64-linux";
      overlays = [ self.overlay ];
    };
    src = pkgs.nix-gitignore.gitignoreSourcePure [./.gitignore] ./.;
  in {
    overlay = final: prev: {
      zarybnicky-com-modules = final.callPackage ./modules.nix {};
    };

    packages.x86_64-linux = {
      inherit (pkgs) zarybnicky-com-modules;
      zarybnicky-com = pkgs.callPackage ./builder.nix {
        vault = /home/inuits/Vault;
      };
    };

    devShell.x86_64-linux = pkgs.mkShell {
      name = "nix-devShell";
      nativeBuildInputs = [
        pkgs.nodePackages.gatsby-cli
      ];
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
              "/".root = pkgs.callPackage ./builder.nix {
                vault = cfg.vaultDir;
              };
              "/".extraConfig = ''
                etag off;
                add_header Last-Modified "";
                add_header etag W/"${builtins.substring 11 32 "${pkgs.zarybnicky-com}"}";
              '';
              "/static".root = cfg.stateDir;
            };
          };
        };
      };
    };
  };
}
