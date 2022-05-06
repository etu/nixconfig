{
  stdenv,
  fetchFromGitHub,
}:
let
  pname = "WiringPi";
  version = "2.61-1";
in
stdenv.mkDerivation {
  inherit pname version;

  src = fetchFromGitHub {
    owner = pname;
    repo = pname;
    rev = version;
    sha256 = "sha256-VxAaPhaPXd9xYt663Ju6SLblqiSLizauhhuFqCqbO5M=";
  };

  makeFlags = [
    "DESTDIR=${placeholder "out"}"
    "PREFIX="
    "LDCONFIG=:"
  ];

  sourceRoot = "source/wiringPi";
}
