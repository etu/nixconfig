{
  osConfig,
  pkgs,
  ...
}:
{
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
  };

  # Install my defined firefox package
  programs.firefox = {
    enable = true;
    inherit (osConfig.etu.graphical.firefox) package;

    languagePacks = [
      "sv-SE"
      "en-GB"
    ];

    profiles.default = {
      # Install extensions from NixOS configuration
      extensions.packages = osConfig.etu.graphical.firefox.extensions;

      isDefault = true;
      search.default = "ddg";
      search.force = true;
      search.engines = {
        bing.metaData.hidden = true;
        GitHub = {
          urls = [ { template = "https://github.com/search?q={searchTerms}"; } ];
          icon = "https://github.com/fluidicon.png";
          updateInterval = 7 * 24 * 60 * 60 * 1000;
          definedAliases = [ "@gh" ];
        };
        google.metaData.alias = "@g";
        "Nix Packages" = {
          urls = [ { template = "https://search.nixos.org/packages?type=packages&query={searchTerms}"; } ];
          icon = "${pkgs.nixos-icons}/share/icons/hicolor/scalable/apps/nix-snowflake.svg";
          definedAliases = [ "@np" ];
        };
        "NixOS Wiki" = {
          urls = [ { template = "https://wiki.nixos.org/w/index.php?search={searchTerms}"; } ];
          icon = "${pkgs.nixos-icons}/share/icons/hicolor/scalable/apps/nix-snowflake.svg";
          definedAliases = [ "@nw" ];
        };
        Perplexity = {
          urls = [ { template = "https://www.perplexity.ai/?q={searchTerms}"; } ];
          icon = "https://www.perplexity.ai/favicon.ico";
          updateInterval = 7 * 24 * 60 * 60 * 1000;
          definedAliases = [ "@p" ];
        };
      };
      settings = {
        # Extensions are managed with Nix, so don't update.
        "extensions.update.autoUpdateDefault" = false;
        "extensions.update.enabled" = false;

        # Sync
        "services.sync.username" = osConfig.etu.user.email;

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

        # Enable userChrome customisations
        "toolkit.legacyUserProfileCustomizations.stylesheets" = true;

        # Enable firefox built in vertical tabs
        "sidebar.verticalTabs" = true;

        # Set default website appearance setting to dark (0) rather than the
        # default: automatic (2) or light (1).
        "layout.css.prefers-color-scheme.content-override" = 0;

        # Disable the annoying suggestions to do things with chatgpt by selecting text
        "browser.ml.enable" = false;
        "browser.ml.chat.enabled" = false;
      };
    };
  };
}
