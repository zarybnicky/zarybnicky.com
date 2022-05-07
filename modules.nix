{ mkYarnModules
, fixup_yarn_lock
, nix-gitignore
, libsass
, nodejs
, nodePackages
, pkg-config
, python3
, vips
, autoPatchelfHook
, musl
, stdenv
, yarn
, git
, lib
, runCommand
, yarn2nix
, callPackage
, writeText
}:

let
  packageJSON = ./package.json;
  yarnLock = ./yarn.lock;
  mkYarnNix = { yarnLock, flags ? [] }:
    runCommand "yarn.nix" {}
    "${yarn2nix}/bin/yarn2nix --lockfile ${yarnLock} --no-patch --builtin-fetchgit ${lib.escapeShellArgs flags} > $out";
  yarnNix = mkYarnNix { inherit yarnLock; };

  offlineCache =
    let
      pkg = callPackage yarnNix { };
    in
      pkg.offline_cache;

  defaultYarnFlags = [
    "--offline"
    "--frozen-lockfile"
    "--ignore-engines"
    "--ignore-scripts"
  ];

in
stdenv.mkDerivation {
  name = "zarybnicky.com-modules-1.0";
  pname = "zarybnicky.com";
  version = "1.0";

  dontUnpack = true;
  dontInstall = true;
  autoPatchelfIgnoreMissingDeps = true;
  nativeBuildInputs = [ yarn nodejs git ];
  buildInputs = [ autoPatchelfHook nodePackages.node-gyp python3 pkg-config vips.dev ];

  buildPhase = ''
    export HOME=$PWD/yarn_home

    runHook preBuild
    cp ${packageJSON} ./package.json
    cp ${yarnLock} ./yarn.lock
    chmod +w ./yarn.lock
    yarn config --offline set yarn-offline-mirror ${offlineCache}
    # Do not look up in the registry, but in the offline cache.
    ${fixup_yarn_lock}/bin/fixup_yarn_lock yarn.lock
    yarn install ${lib.escapeShellArgs defaultYarnFlags}

    pushd node_modules/sharp
    export npm_config_nodedir=${nodejs}
    mkdir -p "$HOME/.cache/node-gyp/${nodejs.version}/include"
    echo 9 > "$HOME/.cache/node-gyp/${nodejs.version}/installVersion"
    ln -sfv "${nodejs}/include/*" "$HOME/.cache/node-gyp/${nodejs.version}/include/"

    sed -i "s@'use_global_libvips': '.*@'use_global_libvips': 'true'@" binding.gyp
    sed -i 's@PKG_CONFIG_PATH="<\(pkg_config_path\)"@@' binding.gyp
    sed -i 's@const libc = \\(*@const libc = ""@' lib/platform.js
    node-gyp rebuild
    popd

    pushd node_modules/gatsby
    sed -i -e '/fs.copy(srcDir/,+3d' -e 's/!oldPluginsHash || pluginsHash !== oldPluginsHash || cacheIsCorrupt/false/' dist/services/initialize.js
    rm dist/services/initialize.js.map
    popd

    autoPatchelf node_modules/sharp

    mkdir $out
    mv node_modules $out/
    patchShebangs $out
    runHook postBuild
  '';
}
