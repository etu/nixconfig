{ pkgs, ... }:
pkgs.stdenv.mkDerivation {
  pname = "jivetalking";
  version = "0.3.0";

  src = pkgs.fetchurl {
    url = "https://github.com/linuxmatters/jivetalking/releases/download/0.3.0/jivetalking-linux-amd64";
    hash = "sha256:1d6lrqvi5kcsc01w8z1lfsjc760g36zm8vjfq1av8i3xq91azfi4";
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
