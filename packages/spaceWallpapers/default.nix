{
  pkgs,
  # Assorted images from: https://wallhaven.cc/search?q=space&atleast=2560x2880
  images ? [
    (pkgs.fetchurl {
      url = "https://w.wallhaven.cc/full/0w/wallhaven-0wryzq.jpg";
      hash = "sha256-61DKr+YemXEraAfzxwddMzFCUZAXFn11LUqW2TdGi8w=";
    })
    (pkgs.fetchurl {
      url = "https://w.wallhaven.cc/full/6o/wallhaven-6ol5z7.jpg";
      hash = "sha256-5cpf4DoaMcN96ZnQVmrNNxd5AZ6kxLDEqyrNRFoYxB4=";
    })
    (pkgs.fetchurl {
      url = "https://w.wallhaven.cc/full/e7/wallhaven-e78y9r.jpg";
      hash = "sha256-kR9gmxcQavDlqD7jZYjOa7cWsfqK/sN6e7Q+aJ0jjJg=";
    })
    (pkgs.fetchurl {
      url = "https://w.wallhaven.cc/full/4d/wallhaven-4ddjr3.jpg";
      hash = "sha256-k/d9fu5ZfjBibpdBE2ghna5JRTQ64fn8nlslr72cIfg=";
    })
    (pkgs.fetchurl {
      url = "https://w.wallhaven.cc/full/dp/wallhaven-dpl57g.png";
      hash = "sha256-fUkbZwvm440MKVdro8X3+qx4jgYpjYWu9WkulL8C5Mo=";
    })
  ]
  ++ extraImages,
  extraImages ? [ ],
  ...
}:
let
  # Build a list of ln commands to symlink the images.
  symlinkCommandList = map (image: "ln -vs ${image} $out/${image.name}") images;

  # Concat these for use in the shell script.
  symlinkCommands = builtins.concatStringsSep "\n" symlinkCommandList;
in
pkgs.runCommandNoCC "spaceWallpapers" { } ''
  mkdir -p $out

  ${symlinkCommands}
''
