_: {
  # Central home for temporary overlay hacks — one place to audit
  # what's waiting on upstream and remove once fixed.

  nixpkgs.overlays = [
    (_: prev: {
      # matrix-appservice-irc 4.0.0 uses nedb which calls util.isDate,
      # removed in Node.js 24. Override to use Node.js 22 until the
      # package is updated.
      #
      # Track: https://github.com/NixOS/nixpkgs/pull/541566
      matrix-appservice-irc = prev.matrix-appservice-irc.overrideAttrs (oa: {
        nativeBuildInputs = builtins.map (
          dep: if dep == prev.nodejs-slim then prev.nodejs-slim_22 else dep
        ) oa.nativeBuildInputs;
      });

      # Patch flatpak to fix the glycin-svg / prlimit spawn issue
      # (flatpak PR #6721). The fix passes run-environ as execvp envp
      # instead of injecting it into the sandbox via --env, so NixOS
      # store paths don't pollute the sub-sandbox PATH.
      #
      # Track: https://github.com/flatpak/flatpak/pull/6721
      flatpak = prev.flatpak.overrideAttrs (oa: {
        patches = (oa.patches or [ ]) ++ [ ./patches/fix-pr-6721-spawn-path.patch ];
      });
    })
  ];
}
