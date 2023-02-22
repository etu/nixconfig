{
  stdenv,
  graphicsmagick,
  runCommand,
  sunpaper,
}: let
  # Upper left corner of rectangle.
  base.x = 3835;
  base.y = 35;
  # Add 720p to the coordinates to find the lower right corner.
  rect.xy0 = "${toString base.x},${toString base.y}";
  rect.xy1 = "${toString (base.x + 1280)},${toString (base.y + 720)}";
  # Add some pixels to the base coordinates to place the text nicely.
  text.xy = "${toString (base.x + 1115)},${toString (base.y + 60)}";
in
  stdenv.mkDerivation {
    pname = "sway-wallpaper";
    inherit (sunpaper) version src;

    installPhase = ''
      mkdir -p $out

      # Resize for normal background
      ${graphicsmagick}/bin/gm convert -crop 7680x2160+0+375 -resize 5120x1440 ./images/Lakeside/5.jpg $out/default.jpg

      # Resize a darker variation for screen locker
      ${graphicsmagick}/bin/gm convert -crop 7680x2160+0+375 -resize 5120x1440 ./images/Lakeside/8.jpg $out/dark.jpg

      # Draw a 720p rectangle on top
      ${graphicsmagick}/bin/gm convert -fill '#FFFFFFBB' -draw 'rectangle ${rect.xy0} ${rect.xy1}' $out/default.jpg 720pfigure.jpg

      # Draw a text on top of this
      ${graphicsmagick}/bin/gm convert -fill '#FFFFFF' -pointsize 72 -draw 'text ${text.xy} "720p"' 720pfigure.jpg $out/720pfigure.jpg
    '';
  }
