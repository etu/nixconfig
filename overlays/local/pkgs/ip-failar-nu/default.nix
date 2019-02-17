{ stdenv, go, fetchFromGitHub }:

stdenv.mkDerivation rec {
  name = "ip-failar-nu-${version}";
  version = "20180318";

  nativeBuildInputs = [ go ];

  installPhase = ''
    install -D ip-failar-nu $out/bin/ip-failar-nu
  '';

  src = fetchFromGitHub {
    owner = "etu";
    repo = "ip.failar.nu";
    rev = "925218c6615659e56faabbab64146dff8c38b55c";
    sha256 = "0qmvya8ilgj3y38dxy7qk64cxpfjrbp78iihj8nl97iqq29s5lf0";
    fetchSubmodules = true;
  };
}
