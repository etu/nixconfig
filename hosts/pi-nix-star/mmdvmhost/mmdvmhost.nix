{
  stdenv,
  fetchFromGitHub,
  arduipi_oled,
  # arduipi_oled_fork,
  i2c-tools,
  wiringPi,
  wiringPiDev,
}:
let
  arduipiOled = arduipi_oled;
in
stdenv.mkDerivation {
  pname = "MMDVMHost";
  version = "2022-04-16";

  src = fetchFromGitHub {
    owner = "g4klx";
    repo = "MMDVMHost";
    rev = "33939d8cf58b4969a1278cde7760ab25bb64fda3";
    sha256 = "sha256-ZCHtaKq9mJKy9AJDYtOAdDQE47I9+zN5Zt8HUSxHBLY=";
  };

  buildInputs = [
    arduipiOled
    wiringPi
    wiringPiDev
  ];
  nativeBuildInputs = [ i2c-tools ];

  makefile = "Makefile.Pi.OLED";

  prePatch = ''
    substituteInPlace Makefile.Pi.OLED \
      --replace "-g " "" \
      --replace "-O3" "-O2" \
      --replace "/usr/local/bin" "$out/bin" \
      --replace "/usr/local/lib" "${arduipiOled}/lib" \
      --replace "/usr/local/include" "${arduipiOled}/include/ArduiPi_OLED" \
      --replace "-lArduiPi_OLED -lpthread" "-L${arduipiOled}/lib -L${i2c-tools}/lib -li2c -lArduiPi_OLED -lpthread"
  '';

  preInstall = ''
    mkdir -p $out/bin $out/share

    cp -v DMRIds.dat $out/share
  '';
}
