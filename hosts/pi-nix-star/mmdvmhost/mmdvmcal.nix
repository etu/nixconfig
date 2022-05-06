{
  stdenv,
  fetchFromGitHub,
}:
stdenv.mkDerivation {
  pname = "MMDVMCal";
  version = "2021-11-01";

  src = fetchFromGitHub {
    owner = "g4klx";
    repo = "MMDVMCal";
    rev = "149a23561af2665abd00b4e9c0aeba646bb1198e";
    sha256 = "sha256-IxwhyIlWynHleoM0gHTAY9tccHzFrj+yy/JSzpxJb3o=";
  };

  prePatch = ''
    substituteInPlace Makefile \
     --replace "/usr/local/bin" "$out/bin"
  '';

  preInstall = ''
    mkdir -p $out/bin
  '';
}
