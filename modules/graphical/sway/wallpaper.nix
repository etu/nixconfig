{
  stdenv,
  fetchFromGitHub,
  graphicsmagick,
}:
stdenv.mkDerivation {
  pname = "wallpaper";
  version = "2021-04-01";

  src = fetchFromGitHub {
    owner = "hexive";
    repo = "sunpaper";
    rev = "8d518dfddb5e80215ef3b884ff009df1d4bb74c2";
    sha256 = "sha256-sCG7igD2ZwfHoRpR3Kw7dAded4hG2RbMLR/9nH+nZh8=";
  };

  installPhase = let
    # upper left corner of rectangle.
    base = {
      x = 3835;
      y = 35;
    };
    rect = {
      xy0 = "${toString base.x},${toString base.y}";
      # Add 720p to the coordinates to find the lower right corner.
      xy1 = "${toString (base.x + 1280)},${toString (base.y + 720)}";
    };
    # Add some pixels to the base coordinates to place the text nicely.
    text.xy = "${toString (base.x + 1115)},${toString (base.y + 60)}";
  in ''
    mkdir -p $out

    # Resize for normal background
    ${graphicsmagick}/bin/gm convert -crop 7680x2160+0+375 -resize 5120x1440 images/Lakeside/5.jpg $out/default.jpg

    # Resize a darker variation for screen locker
    ${graphicsmagick}/bin/gm convert -crop 7680x2160+0+375 -resize 5120x1440 images/Lakeside/8.jpg $out/dark.jpg

    # Draw a 720p rectangle on top
    ${graphicsmagick}/bin/gm convert -fill '#FFFFFFBB' -draw 'rectangle ${rect.xy0} ${rect.xy1}' $out/default.jpg 720pfigure.jpg

    # Draw a text on top of this
    ${graphicsmagick}/bin/gm convert -fill '#FFFFFF' -pointsize 72 -draw 'text ${text.xy} "720p"' 720pfigure.jpg $out/720pfigure.jpg
  '';
}
