{
  runCommandNoCC,
  fetchurl,
  ...
}: let
  # Asorted images from: https://wallhaven.cc/search?q=space&atleast=2560x2880
  images = {
    wallhaven-8x1odo = fetchurl {
      url = "https://w.wallhaven.cc/full/8x/wallhaven-8x1odo.jpg";
      hash = "sha256-C3lAjdp0Ey0MP2+BNz90tVvlwN1GnWIpqPjJfNYpAQU=";
    };
    wallhaven-0wryzq = fetchurl {
      url = "https://w.wallhaven.cc/full/0w/wallhaven-0wryzq.jpg";
      hash = "sha256-61DKr+YemXEraAfzxwddMzFCUZAXFn11LUqW2TdGi8w=";
    };
    wallhaven-6ol5z7 = fetchurl {
      url = "https://w.wallhaven.cc/full/6o/wallhaven-6ol5z7.jpg";
      hash = "sha256-5cpf4DoaMcN96ZnQVmrNNxd5AZ6kxLDEqyrNRFoYxB4=";
    };
    wallhaven-e78y9r = fetchurl {
      url = "https://w.wallhaven.cc/full/e7/wallhaven-e78y9r.jpg";
      hash = "sha256-kR9gmxcQavDlqD7jZYjOa7cWsfqK/sN6e7Q+aJ0jjJg=";
    };
    wallhaven-4ddjr3 = fetchurl {
      url = "https://w.wallhaven.cc/full/4d/wallhaven-4ddjr3.jpg";
      hash = "sha256-k/d9fu5ZfjBibpdBE2ghna5JRTQ64fn8nlslr72cIfg=";
    };
    wallhaven-dpl57g = fetchurl {
      url = "https://w.wallhaven.cc/full/dp/wallhaven-dpl57g.png";
      hash = "sha256-fUkbZwvm440MKVdro8X3+qx4jgYpjYWu9WkulL8C5Mo=";
    };
  };
in
  runCommandNoCC "spaceWallpapers" {} ''
    mkdir -p $out/share/spaceWallpapers

    ln -vs ${images.wallhaven-8x1odo} $out/share/spaceWallpapers/wallhaven-8x1odo.jpg
    ln -vs ${images.wallhaven-0wryzq} $out/share/spaceWallpapers/wallhaven-0wryzq.jpg
    ln -vs ${images.wallhaven-6ol5z7} $out/share/spaceWallpapers/wallhaven-6ol5z7.jpg
    ln -vs ${images.wallhaven-e78y9r} $out/share/spaceWallpapers/wallhaven-e78y9r.jpg
    ln -vs ${images.wallhaven-4ddjr3} $out/share/spaceWallpapers/wallhaven-4ddjr3.jpg
    ln -vs ${images.wallhaven-dpl57g} $out/share/spaceWallpapers/wallhaven-dpl57g.png
  ''
