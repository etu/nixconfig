{
  stdenv,
  wiringPi,
}:
stdenv.mkDerivation {
  pname = "${wiringPi.pname}Dev";
  inherit (wiringPi) version src makeFlags;

  buildInputs = [ wiringPi ];

  sourceRoot = "source/devLib";
}
