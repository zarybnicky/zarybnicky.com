{ lib, buildGoModule, fetchFromGitHub, installShellFiles }:

buildGoModule rec {
  pname = "hugo-obsidian";
  version = "2.13";

  src = fetchFromGitHub {
    owner = "jackyzha0";
    repo = pname;
    rev = "v${version}";
    sha256 = "sha256-uTjH+v2/ix4uV5SY117Y4EVAfhvNmNkNr17OvodSk/M=";
  };

  vendorSha256 = "sha256-mAguRor2wMVkvhrDi0jzdMK1jGwXHVNqDFEQgJFe+Q0=";

  doCheck = false;

  proxyVendor = true;

  tags = [ "extended" ];

  subPackages = [ "." ];

  nativeBuildInputs = [ installShellFiles ];
}
