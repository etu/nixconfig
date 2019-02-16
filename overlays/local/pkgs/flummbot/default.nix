{ stdenv, go, fetchFromGitHub }:

stdenv.mkDerivation rec {
  pname = "flummbot";
  version = "20180703";

  nativeBuildInputs = [ go ];

  installPhase = ''
    install -D flummbot $out/bin/flummbot
  '';

  src = fetchFromGitHub {
    owner = "etu";
    repo = "flummbot";
    rev = "b32f569b6040ebdc7b258c95be4e17e41a152d59";
    sha256 = "0hw8dprji4l34dcpwhcq4nmnqwwbv33ppv7j4w5k2gkz4894y8nz";
    fetchSubmodules = true;
  };
}
