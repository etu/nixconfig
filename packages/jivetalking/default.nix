{ pkgs, ... }:
pkgs.stdenv.mkDerivation {
  pname = "jivetalking";
  version = "0.6.1";

  src = pkgs.fetchurl {
    url = "https://github.com/linuxmatters/jivetalking/releases/download/0.6.1/jivetalking-linux-amd64";
    hash = "sha256:16p7prymy5idg1h5cllvw5yssra6lrgy6hyl5m88h825365y8y8i";
  };

  nativeBuildInputs = [ pkgs.autoPatchelfHook ];
  buildInputs = [ pkgs.stdenv.cc.cc.lib ];

  dontUnpack = true;
  dontBuild = true;

  installPhase = ''
    install -Dm755 $src $out/bin/jivetalking
  '';

  meta = {
    description = "Raw microphone recordings into broadcast-ready audio in one command. No configuration, and no surprises 🕺";
    license = pkgs.lib.licenses.gpl3Only;
    platforms = [ "x86_64-linux" ];
  };
}
