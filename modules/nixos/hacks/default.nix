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

      # Update cloudflared 2026.10.1 -> 2026.10.3 ahead of nixpkgs. The
      # vendorHash is unchanged. Only applies to 2026.10.1, so it's a no-op
      # once nixpkgs moves on.
      #
      # Track: https://github.com/NixOS/nixpkgs/pull/576762
      cloudflared =
        if prev.cloudflared.version == "2026.10.1" then
          prev.cloudflared.overrideAttrs (_: rec {
            version = "2026.10.3";
            src = prev.fetchFromGitHub {
              owner = "cloudflare";
              repo = "cloudflared";
              tag = version;
              hash = "sha256-k1OMkHHItOT5VHhQqgC8ODm/RCJEtwtZi0Lxyf/esLg=";
            };
          })
        else
          prev.cloudflared;

      ## matrix-appservice-irc 4.0.0 uses nedb which calls util.isDate,
      ## removed in Node.js 24. Override to use Node.js 22 until the
      ## package is updated.
      ##
      ## Track: https://github.com/NixOS/nixpkgs/pull/541566
      #matrix-appservice-irc = prev.matrix-appservice-irc.overrideAttrs (oa: {
      #  nativeBuildInputs = builtins.map (
      #    dep: if dep == prev.nodejs-slim then prev.nodejs-slim_22 else dep
      #  ) oa.nativeBuildInputs;
      #});

      ## Patch flatpak to fix the glycin-svg / prlimit spawn issue
      ## (flatpak PR #6721). The fix passes run-environ as execvp envp
      ## instead of injecting it into the sandbox via --env, so NixOS
      ## store paths don't pollute the sub-sandbox PATH.
      ##
      ## Track: https://github.com/flatpak/flatpak/pull/6721
      #flatpak = prev.flatpak.overrideAttrs (oa: {
      #  patches = (oa.patches or [ ]) ++ [ ./patches/fix-pr-6721-spawn-path.patch ];
      #});
    })
  ];
}
