{ stdenv
, nix-gitignore
, gatsby-builder-modules
, libsass
, vault
}:

stdenv.mkDerivation {
  name = "zarybnicky.com-gatsby";
  src = nix-gitignore.gitignoreSourcePure [../.gitignore] ./.;
  phases = "unpackPhase buildPhase";
  buildInputs = [ gatsby-builder-modules libsass ];
  buildPhase = ''
  mkdir -p $out
  mkdir .cache
  ln -s ${gatsby-builder-modules}/node_modules .
  cp -r node_modules/gatsby/cache-dir/* .cache
  chmod -R ug+w .cache
  VAULT_LOCATION=${vault} HOME=$(pwd) node_modules/.bin/gatsby build --no-color
  mv public/* $out/
'';
}
