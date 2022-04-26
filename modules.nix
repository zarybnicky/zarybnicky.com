{ yarn2nix-moretea
, nix-gitignore
, libsass
, nodejs
, nodePackages
, pkg-config
, python3
, glib
, vips
}:

yarn2nix-moretea.mkYarnModules {
  packageJSON = ./package.json;
  yarnLock = ./yarn.lock;
  name = "zarybnicky.com-modules-1.0";
  pname = "zarybnicky.com";
  version = "1.0";
  preBuild = "export npm_config_nodedir=${nodejs}";
  pkgConfig = {
    sharp = {
      nativeBuildInputs = [ nodePackages.node-gyp python3 pkg-config ];
      buildInputs = [ glib vips ];
      postInstall = ''
        ls ${nodePackages.node-gyp}/bin
        ${nodePackages.node-gyp}/bin/node-gyp rebuild
      '';
    };
    node-sass = {
      nativeBuildInputs = [pkg-config python3];
      buildInputs = [libsass];
      postInstall = ''
        LIBSASS_EXT=auto yarn --offline run build
        rm build/config.gypi
      '';
    };
    gatsby = {
      postInstall = ''
        sed -i -e '/fs.copy(srcDir/,+3d' -e 's/!oldPluginsHash || pluginsHash !== oldPluginsHash || cacheIsCorrupt/false/' dist/services/initialize.js
        rm dist/services/initialize.js.map
      '';
    };
  };
}
