{
  config,
  lib,
  pkgs,
  ...
}: {
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
      programs.browserpass.browsers = ["firefox"];

      # Make firefox the default browser
      xdg.mimeApps = {
        enable = true;
        defaultApplications = {
          "text/html" = ["firefox.desktop"];
          "x-scheme-handler/http" = ["firefox.desktop"];
          "x-scheme-handler/https" = ["firefox.desktop"];
          "x-scheme-handler/about" = ["firefox.desktop"];
          "x-scheme-handler/unknown" = ["firefox.desktop"];
        };
      }; # END xdg.mimeApps

      # Install my defined firefox package
      programs.firefox = {
        enable = true;
        inherit (config.etu.graphical.firefox) package;

        profiles.default = {
          # Install extensions.
          extensions = [
            config.nur.repos.rycee.firefox-addons.browserpass
            config.nur.repos.rycee.firefox-addons.elasticvue
            config.nur.repos.rycee.firefox-addons.facebook-container
            config.nur.repos.rycee.firefox-addons.multi-account-containers
            config.nur.repos.rycee.firefox-addons.privacy-badger
            config.nur.repos.rycee.firefox-addons.streetpass-for-mastodon
            config.nur.repos.rycee.firefox-addons.swedish-dictionary
            config.nur.repos.rycee.firefox-addons.terms-of-service-didnt-read
            config.nur.repos.rycee.firefox-addons.ublock-origin
          ];

          isDefault = true;
          search.default = "DuckDuckGo";
          search.force = true;
          search.engines = {
            Bing.metaData.hidden = true;
            GitHub = {
              urls = [{template = "https://github.com/search?q={searchTerms}";}];
              iconUpdateURL = "https://github.com/fluidicon.png";
              updateInterval = 7 * 24 * 60 * 60 * 1000;
              definedAliases = ["@gh"];
            };
            Google.metaData.alias = "@g";
            "Nix Packages" = {
              urls = [{template = "https://search.nixos.org/packages?type=packages&query={searchTerms}";}];
              icon = "${pkgs.nixos-icons}/share/icons/hicolor/scalable/apps/nix-snowflake.svg";
              definedAliases = ["@np"];
            };
            "NixOS Wiki" = {
              urls = [{template = "https://wiki.nixos.org/w/index.php?search={searchTerms}";}];
              icon = "${pkgs.nixos-icons}/share/icons/hicolor/scalable/apps/nix-snowflake.svg";
              definedAliases = ["@nw"];
            };
            Perplexity = {
              urls = [{template = "https://www.perplexity.ai/?q={searchTerms}";}];
              iconUpdateURL = "https://www.perplexity.ai/favicon.ico";
              updateInterval = 7 * 24 * 60 * 60 * 1000;
              definedAliases = ["@p"];
            };
          };
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

            # Enable userChrome customisations
            "toolkit.legacyUserProfileCustomizations.stylesheets" = true;

            # Enable firefox built in vertical tabs
            "sidebar.verticalTabs" = true;
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
