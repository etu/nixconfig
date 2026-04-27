{ pkgs, ... }:
pkgs.stdenv.mkDerivation {
  pname = "jivetalking";
  version = "0.3.1";

  src = pkgs.fetchurl {
    url = "https://github.com/linuxmatters/jivetalking/releases/download/0.3.1/jivetalking-linux-amd64";
    hash = "sha256:16yb4zahi0rp43n3i08z2dpfy2z06h9h4bsmn8md0a68g0q9p4w1";
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
