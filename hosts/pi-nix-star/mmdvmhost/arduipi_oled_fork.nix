{
  stdenv,
  fetchFromGitHub,
  i2c-tools,
}:
stdenv.mkDerivation {
  pname = "ArduiPi_OLED";
  version = "2018-12-23";

  src = fetchFromGitHub {
    owner = "jonesman";
    repo = "ArduiPi_OLED";
    rev = "cafc263dcd7cb45f38469140cec82a4bd85f6f83";
    sha256 = "sha256-sxEsQMQo/dV+cNSu6KHKhZRzSZDF43QA7wCHkzsPADg=";
  };

  nativeBuildInputs = [ i2c-tools ];

  # The makefile is horrible, let's ignore it!
  buildPhase = ''
    g++ -Wall -fPIC -fno-rtti -Ofast -march=native -mtune=generic -c ArduiPi_OLED.cpp
    g++ -Wall -fPIC -fno-rtti -Ofast -march=native -mtune=generic -c Adafruit_GFX.cpp
    gcc -Wall -fPIC -Ofast -march=native -mtune=generic -c Wrapper.cpp
    g++ -shared -Wl,-soname,libArduiPi_OLED.so.1 -Ofast -march=native -mtune=generic -o libArduiPi_OLED.so.1.0 ArduiPi_OLED.o Adafruit_GFX.o Wrapper.o -li2c
  '';

  installPhase = ''
    mkdir -p $out/lib $out/include/ArduiPi_OLED

    cp -v *.h   $out/include/ArduiPi_OLED/
    cp -v *.so* $out/lib

    ln -s ./libArduiPi_OLED.so.1.0 $out/lib/libArduiPi_OLED.so
    ln -s ./libArduiPi_OLED.so.1.0 $out/lib/libArduiPi_OLED.so.1
  '';
}
