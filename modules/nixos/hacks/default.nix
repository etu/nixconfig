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

      # Python packages now default to __structuredAttrs, which makes
      # sphinxHook read only the first entry of sphinxBuilders, so khal's man
      # page is never built and the build fails on the missing `man` output.
      # Pass both builders as one string, like upstream's workaround. Only
      # applies while khal still lists them separately, so it's a no-op once
      # the upstream fix reaches nixos-unstable.
      #
      # Track: https://github.com/NixOS/nixpkgs/pull/576662
      khal =
        if
          prev.khal.sphinxBuilders == [
            "html"
            "man"
          ]
        then
          prev.khal.overrideAttrs (_: {
            sphinxBuilders = [ "html man" ];
          })
        else
          prev.khal;

      # aggregateModules now merges a .ko.cer certificate for every external
      # kernel module, but acpi_call copies its module by hand instead of
      # going through Kbuild's modules_install, so no certificate is made and
      # the kernel modules fail to build. Install it like upstream's fix.
      # Only applies while acpi_call still has its own installPhase.
      #
      # Track: https://github.com/NixOS/nixpkgs/pull/575394
      kernelPackagesExtensions = prev.kernelPackagesExtensions ++ [
        (_: kprev: {
          acpi_call =
            if kprev.acpi_call ? installPhase then
              kprev.acpi_call.overrideAttrs (oa: {
                makeFlags = oa.makeFlags ++ [
                  "INSTALL_MOD_PATH=${placeholder "out"}"
                  "INSTALL_MOD_DIR=misc"
                  "DEPMOD=true"
                ];
                installPhase = null;
                postInstall = ''
                  install -D -m755 examples/turn_off_gpu.sh $out/bin/test_discrete_video_off.sh
                '';
              })
            else
              kprev.acpi_call;
        })
      ];

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
