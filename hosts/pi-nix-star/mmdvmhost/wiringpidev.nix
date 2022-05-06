{ stdenv, wiringPi, fetchFromGitHub }:

stdenv.mkDerivation {
  pname = "${wiringPi.pname}Dev";
  inherit (wiringPi) version src makeFlags;

  buildInputs = [ wiringPi ];

  sourceRoot = "source/devLib";
}
