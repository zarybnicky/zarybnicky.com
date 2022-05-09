{ stdenv
, nix-gitignore
, hugo
, hugo-obsidian
, vault
, git
}:

stdenv.mkDerivation {
  name = "zarybnicky.com";
  src = nix-gitignore.gitignoreSourcePure [./.gitignore] ./.;
  phases = "unpackPhase buildPhase";
  nativeBuildInputs = [ hugo hugo-obsidian git ];
  buildPhase = ''
  git init
  git config user.email "you@example.com"
  git config user.name "Your Name"
  git config core.ignorecase true
  git commit --allow-empty -m "Empty"

  ln -s ${vault}/Public content/
  export VAULT=${vault}
  hugo-obsidian -input=$VAULT -output=./assets/indices -index -root=.
  hugo --minify

  mkdir -p $out
  mv public/* $out/
'';
}
