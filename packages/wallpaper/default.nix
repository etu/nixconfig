{
  stdenv,
  graphicsmagick,
  sunpaper,
}:
stdenv.mkDerivation {
  pname = "sway-wallpaper";
  inherit (sunpaper) version src;

  installPhase = ''
    mkdir -p $out

    # Resize a darker variation for screen locker
    ${graphicsmagick}/bin/gm convert -crop 7680x2160+0+375 -resize 5120x1440 ./images/Lakeside/8.jpg $out/lock.jpg
  '';
}
