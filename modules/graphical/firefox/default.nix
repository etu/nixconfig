{ config, lib, pkgs, ... }:
let
  buildFirefoxXpiAddon = { pname, version, addonId, url, sha256, meta ? { }, ... }:
    pkgs.stdenv.mkDerivation {
      inherit meta pname version;
      src = pkgs.fetchurl { inherit url sha256; };

      buildCommand = ''
        dst="$out/share/mozilla/extensions/{ec8030f7-c20a-464f-9b0e-13a3a9e97384}"
        mkdir -p "$dst"
        install -v -m644 "$src" "$dst/${addonId}.xpi"
      '';
    };

in {
  options.etu.graphical.firefox = {
    enable = lib.mkEnableOption "Enable graphical firefox settings";
    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.firefox-bin;
      description = "Firefox package to use.";
      readOnly = true;
    };
  };

  config = lib.mkIf config.etu.graphical.firefox.enable {
    # Configure firefox for my users home-manager (if it's enabled).
    home-manager.users.${config.etu.user.username} = lib.mkIf config.etu.user.enable {
      # Enable browserpass integration
      programs.browserpass.enable = true;
      programs.browserpass.browsers = [ "firefox" ];

      # Make firefox the default browser
      xdg.mimeApps = {
        enable = true;
        defaultApplications = {
          "text/html" = [ "firefox.desktop" ];
          "x-scheme-handler/http" = [ "firefox.desktop" ];
          "x-scheme-handler/https" = [ "firefox.desktop" ];
          "x-scheme-handler/about" = [ "firefox.desktop" ];
          "x-scheme-handler/unknown" = [ "firefox.desktop" ];
        };
      }; # END xdg.mimeApps

      # Install my defined firefox package
      programs.firefox = {
        enable = true;
        package = config.etu.graphical.firefox.package;

        # Install extensions.
        extensions = map buildFirefoxXpiAddon (lib.attrValues (lib.importJSON ./extensions.json));

        profiles.default = {
          isDefault = true;
          settings = {
            # Extensions are managed with Nix, so don't update.
            "extensions.update.autoUpdateDefault" = false;
            "extensions.update.enabled" = false;

            # Sync
            "services.sync.username" = config.etu.user.email;

            # Do not sync extensions.
            "services.sync.engine.addons" = false;

            # Middle click to scroll
            "general.autoScroll" = true;

            # Restore previous windows and tabs.
            "browser.startup.page" = 3;

            # Never show bookmarks toolbar
            "browser.toolbars.bookmarks.visibility" = "never";

            # Privacy enhancements
            "browser.newtabpage.activity-stream.feeds.telemetry" = false;
            "browser.newtabpage.activity-stream.telemetry" = false;
            "browser.newtabpage.activity-stream.feeds.snippets" = false;
            "browser.newtabpage.activity-stream.feeds.section.topstories" = false;
            "browser.newtabpage.activity-stream.section.highlights.includePocket" = false;
            "browser.newtabpage.activity-stream.showSponsored" = false;
            "browser.newtabpage.activity-stream.feeds.discoverystreamfeed" = false;
            "browser.newtabpage.activity-stream.showSponsoredTopSites" = false;

            # Improve performance
            "gfx.webrender.all" = true;

            # Enable browser debugging
            "devtools.chrome.enabled" = true;

            # Do Not Track header
            "privacy.donottrackheader.enabled" = true;
            "privacy.donottrackheader.value" = 1;
          };
        };
      };
    };

    # Enable persistence for firefox files.
    etu.base.zfs.user.directories = [
      ".mozilla/firefox/default"
    ];
  };
}
