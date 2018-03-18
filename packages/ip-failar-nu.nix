{ stdenv, go, fetchFromGitHub }:

stdenv.mkDerivation rec {
  name = "ip-failar-nu";
  version = "20180318";

  nativeBuildInputs = [ go ];

  installPhase = ''
    mkdir -p $out/bin
    cp ip-failar-nu $out/bin
  '';

  src = fetchFromGitHub {
    owner = "etu";
    repo = "ip.failar.nu";
    rev = "925218c6615659e56faabbab64146dff8c38b55c";
    githubBase = "git.elis.nu";
    fetchSubmodules = true;
    sha256 = "0qmvya8ilgj3y38dxy7qk64cxpfjrbp78iihj8nl97iqq29s5lf0";
  };
}
