{
  config,
  lib,
  osConfig,
  pkgs,
  emacsPackageBuilder,
  emacsTreesitGrammars,
  emacsExtraPackages,
  ...
}:
let
  # Load the config file and substitute variables in pure Nix (avoiding IFD)
  emacsConfigRaw = builtins.readFile ./config.el;
  
  # Perform substitutions in pure Nix
  emacsConfigSubstituted = builtins.replaceStrings
    [
      "@treesitGrammars@"
      "@dataPrefix@"
      "@extraConfig@"
      "@fontname@"
      "@fontsize@"
    ]
    [
      "${emacsTreesitGrammars}/lib"
      osConfig.etu.dataPrefix
      (lib.concatStringsSep "\n\n" osConfig.etu.base.emacs.extraConfig)
      osConfig.etu.graphical.theme.fonts.monospace
      (toString osConfig.etu.graphical.theme.fonts.size)
    ]
    emacsConfigRaw;
  
  # Write the substituted config to a file for loading
  emacsConfigFile = pkgs.writeText "config.el" emacsConfigSubstituted;
in
{
  # Install language servers and tools as home packages
  home.packages = emacsExtraPackages;

  # Enable emacs in home-manager using programs.emacs
  programs.emacs = {
    enable = true;
    # Use the package built in nixos module (where overlay is available)
    package = emacsPackageBuilder emacsConfigSubstituted;

    # Initialize with the config file
    extraConfig = ''
      ;; Add a startup hook that logs the startup time to the messages buffer
      (add-hook 'emacs-startup-hook
          (lambda ()
              (message "Emacs ready in %s with %d garbage collections."
                  (format "%.2f seconds"
                      (float-time
                          (time-subtract after-init-time before-init-time)))
                      gcs-done)))

      ;; Increase the threshold to reduce the amount of garbage collections made
      ;; during startups.
      (let ((gc-cons-threshold (* 50 1000 1000))
            (gc-cons-percentage 0.6)
            (file-name-handler-alist nil))

        ;; Load config
        (load-file "${emacsConfigFile}"))
    '';
  };

  # Enable emacs service (daemon)
  services.emacs = {
    enable = true;
    client.enable = true;
  };
}
