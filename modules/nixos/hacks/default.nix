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

      # Update cloudflared 2026.10.3 -> 2026.10.7 ahead of nixpkgs. The Go
      # dependencies changed, so the vendorHash is new too. Only applies to
      # 2026.10.3, so it's a no-op once nixpkgs moves on.
      #
      # Track: https://github.com/NixOS/nixpkgs/pull/581699
      cloudflared =
        if prev.cloudflared.version == "2026.10.3" then
          prev.cloudflared.overrideAttrs (_: rec {
            version = "2026.10.7";
            src = prev.fetchFromGitHub {
              owner = "cloudflare";
              repo = "cloudflared";
              tag = version;
              hash = "sha256-qyGcPCYaa4UT8ws5sUW+SXCp8QWbVniB4QFR8gGE0ds=";
            };
            vendorHash = "sha256-ah+kI+bWIZqsi9U+AJtWME9rfjTr6QpiLQy0RXNlDzc=";
          })
        else
          prev.cloudflared;
    })
  ];
}
