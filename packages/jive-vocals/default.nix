{ pkgs, ... }:
pkgs.stdenv.mkDerivation {
  pname = "jive-vocals";
  version = "0.7.0";

  src = pkgs.fetchurl {
    url = "https://github.com/linuxmatters/jive-vocals/releases/download/0.7.0/jive-vocals-linux-amd64";
    hash = "sha256:144ymgxddnvyna7cwj0pp5b8hfbwb33l82miqgvc94i6hhcb9996";
  };

  nativeBuildInputs = [ pkgs.autoPatchelfHook ];
  buildInputs = [ pkgs.stdenv.cc.cc.lib ];

  dontUnpack = true;
  dontBuild = true;

  installPhase = ''
    install -Dm755 $src $out/bin/jive-vocals
  '';

  meta = {
    description = "Raw microphone recordings into broadcast-ready audio in one command. No configuration, and no surprises 🕺";
    license = pkgs.lib.licenses.gpl3Only;
    platforms = [ "x86_64-linux" ];
  };
}
