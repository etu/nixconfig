{
  stdenv,
  fetchFromGitHub,
  i2c-tools,
}:
stdenv.mkDerivation {
  pname = "ArduiPi_OLED";
  version = "2018-03-29";

  makeFlags = [
    "CC=armv6l-unknown-linux-gnueabihf-gcc"
    "CXX=armv6l-unknown-linux-gnueabihf-g++"
  ];

  nativeBuildInputs = [ i2c-tools ];

  prePatch = ''
    substituteInPlace Makefile \
      --replace " -li2c" ""    \
      --replace "all: ArduiPi_OLED install" "all: ArduiPi_OLED"
  '';

  installPhase = ''
    mkdir -p $out/lib $out/include/ArduiPi_OLED

    cp -v *.h   $out/include/ArduiPi_OLED/
    cp -v *.so* $out/lib

    ln -s ./libArduiPi_OLED.so.1.0 $out/lib/libArduiPi_OLED.so
    ln -s ./libArduiPi_OLED.so.1.0 $out/lib/libArduiPi_OLED.so.1
  '';

  src = fetchFromGitHub {
    owner = "hallard";
    repo = "ArduiPi_OLED";
    rev = "4119148b051a6975816de2319eb6d11f6769499d";
    sha256 = "05ckc4xpvjcnzq2s69j0ik6kbpsa2z8q6k741w88xz85iidcgvrl";
  };
}
