{
  stdenv,
  fetchFromGitHub,
  arduipi_oled,
  i2c-tools,
}:
stdenv.mkDerivation {
  pname = "MMDVMHost";
  version = "2021-08-08";

  src = fetchFromGitHub {
    owner = "g4klx";
    repo = "MMDVMHost";
    rev = "433cb441be4200ef2bc2358129d73d26f515e428";
    sha256 = "0kp60ayi1d8yws2cxg0ggwcik7g7kmxwacharn2z9i30wviinmq6";
  };

  buildInputs = [ arduipi_oled ];
  nativeBuildInputs = [ i2c-tools ];

  makefile = "Makefile.Pi.OLED";
  makeFlags = [
    "CC=armv6l-unknown-linux-gnueabihf-cc"
    "CXX=armv6l-unknown-linux-gnueabihf-c++"
  ];

  prePatch = ''
    substituteInPlace Makefile.Pi.OLED \
      --replace "-g " "" \
      --replace "-O3" "-O2" \
      --replace "/usr/local/bin" "$out/bin" \
      --replace "/usr/local/lib" "${arduipi_oled}/lib" \
      --replace "/usr/local/include" "${arduipi_oled}/include/ArduiPi_OLED" \
      --replace "-lArduiPi_OLED -lpthread" "-L${arduipi_oled}/lib -L${i2c-tools}/lib -li2c -lArduiPi_OLED -lpthread"
  '';

  preInstall = ''
    mkdir -p $out/bin
  '';
}
