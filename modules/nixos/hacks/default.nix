_: {
  # Central home for temporary overlay hacks — one place to audit
  # what's waiting on upstream and remove once fixed.

  nixpkgs.overlays = [
    (_: prev: {
      # svtplay-dl 4.199's tag was moved after nixpkgs recorded its hash, so
      # the source no longer matches (hash mismatch). Pin the source to the
      # commit the tag points at now, by commit so it can't move again. Only
      # applies to 4.199, so it's a no-op for other versions.
      #
      # Track: remove when nixpkgs updates the svtplay-dl hash or version.
      svtplay-dl =
        if prev.svtplay-dl.version == "4.199" then
          prev.svtplay-dl.overrideAttrs (_: {
            src = prev.fetchFromGitHub {
              owner = "spaam";
              repo = "svtplay-dl";
              rev = "de3d12fef022c7872eacd81e30b9913c9720f210"; # tag 4.199
              hash = "sha256-BQzbeKjY4Jf4K5UviwmVqfRXt1DPD9H+O4+4feKQako=";
            };
          })
        else
          prev.svtplay-dl;
    })
  ];
}
