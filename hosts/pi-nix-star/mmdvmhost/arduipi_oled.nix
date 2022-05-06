{
  stdenv,
  fetchFromGitHub,
  i2c-tools,
}:
stdenv.mkDerivation {
  pname = "ArduiPi_OLED";
  version = "2018-03-29";

  src = fetchFromGitHub {
    owner = "hallard";
    repo = "ArduiPi_OLED";
    rev = "4119148b051a6975816de2319eb6d11f6769499d";
    sha256 = "sha256-NO/HWowF/Y4QD+RMg9EXSt81zYxAJqMF/pbJfTthkxU=";
  };

  nativeBuildInputs = [ i2c-tools ];

  # The makefile is horrible, let's ignore it!
  buildPhase = ''
    g++ -Wall -fPIC -fno-rtti -Ofast -march=native -mtune=generic -c ArduiPi_OLED.cpp
    g++ -Wall -fPIC -fno-rtti -Ofast -march=native -mtune=generic -c Adafruit_GFX.cpp
    gcc -Wall -fPIC -Ofast -march=native -mtune=generic -c bcm2835.c
    gcc -Wall -fPIC -Ofast -march=native -mtune=generic -c Wrapper.cpp
    g++ -shared -Wl,-soname,libArduiPi_OLED.so.1 -Ofast -march=native -mtune=generic -o libArduiPi_OLED.so.1.0 ArduiPi_OLED.o Adafruit_GFX.o bcm2835.o Wrapper.o -li2c
  '';

  installPhase = ''
    mkdir -p $out/lib $out/include/ArduiPi_OLED

    cp -v *.h   $out/include/ArduiPi_OLED/
    cp -v *.so* $out/lib

    ln -s ./libArduiPi_OLED.so.1.0 $out/lib/libArduiPi_OLED.so
    ln -s ./libArduiPi_OLED.so.1.0 $out/lib/libArduiPi_OLED.so.1
  '';
}
