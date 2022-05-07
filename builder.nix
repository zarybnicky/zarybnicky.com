{ stdenv
, nix-gitignore
, zarybnicky-com-modules
, vault
}:

stdenv.mkDerivation {
  name = "zarybnicky.com";
  src = nix-gitignore.gitignoreSourcePure [./.gitignore] ./.;
  phases = "unpackPhase buildPhase";
  nativeBuildInputs = [zarybnicky-com-modules];
  buildPhase = ''
  mkdir -p $out .cache
  ln -s ${zarybnicky-com-modules}/node_modules .

  cp -r node_modules/gatsby/cache-dir/* .cache
  chmod -R ug+w .cache
  VAULT_LOCATION=${vault} HOME=$(pwd) node_modules/.bin/gatsby build --verbose --no-color
  find public -name '*.map' -exec sed -i -E 's|\.\./\.\./nix/store/[-.0-9a-z]{59}|.|g' {} \;
  mv public/* $out/
'';
}
