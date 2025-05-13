{
  config,
  lib,
  pkgs,
  ...
}: {
  imports = [
    ./emacs
    ./fish
    ./htop
    ./nix
    ./tmux
    ./sshd
    ./sanoid
    ./syncoid
    ./spell
    ./tailscale
    ./zfs
  ];

  options.etu = {
    stateVersion = lib.mkOption {
      type = lib.types.str;
      example = "22.05";
      description = "The NixOS state version to use for this system";
    };
    dataPrefix = lib.mkOption {
      type = lib.types.str;
      default = "/data";
      description = "The path to where persistent storage happens";
    };
    localPrefix = lib.mkOption {
      type = lib.types.str;
      default = "/data/local";
      description = "The path to where persistent local storage happens";
    };
  };

  config = {
    # Set the nixpkgs inputs path as channel in the NIX_PATH variable.
    nix.nixPath = ["nixpkgs=${pkgs.path}"];

    # Enable base services.
    etu.base = {
      emacs.enable = lib.mkDefault true;
      fish.enable = lib.mkDefault true;
      htop.enable = lib.mkDefault true;
      tmux.enable = lib.mkDefault true;
      nix.enable = lib.mkDefault true;
      sshd.enable = lib.mkDefault true;
      sanoid.enable = lib.mkDefault true;
      spell.enable = lib.mkDefault true;
      tailscale.enable = lib.mkDefault true;
      zfs.enable = lib.mkDefault true;
    };

    # Set your time zone.
    time.timeZone = "Europe/Stockholm";

    # Select internationalisation properties.
    i18n = {
      defaultLocale = "en_US.UTF-8";
      supportedLocales = [
        "all"
      ];
    };

    # Set console font and keymap.
    console.font = "Lat2-Terminus16";
    console.useXkbConfig = true;

    # Keyboard layout used by the console, X11 and other parts.
    services.xserver.xkb.model = config.etu.graphical.xkb-keymap.model;
    services.xserver.xkb.layout = config.etu.graphical.xkb-keymap.layout;
    services.xserver.xkb.options = config.etu.graphical.xkb-keymap.options;
    services.xserver.xkb.variant = config.etu.graphical.xkb-keymap.variant;

    # Enable experimental features in the nix command to make nix
    # search work.
    nix.settings.experimental-features = ["nix-command" "flakes"];

    # Set system state version.
    system.stateVersion = config.etu.stateVersion;

    # Display a diff of installed packages on system activation.
    system.activationScripts.diff = {
      supportsDryActivation = true;
      text = ''
        NO_FORMAT="\033[0m"
        F_BOLD="\033[1m"
        C_LIME="\033[38;5;10m"

        if test -e /run/current-system; then
          echo -e "''${F_BOLD}''${C_LIME}==> diff to current-system ''${NO_FORMAT}"
          ${pkgs.nvd}/bin/nvd --nix-bin-dir=${config.nix.package}/bin diff /run/current-system "$systemConfig"
        fi
      '';
    };

    # Enable doas.
    security.doas.enable = true;

    # Install some command line tools I commonly want available
    environment.systemPackages = [
      # Nice extra command line tools
      pkgs.bat # "bat - cat with wings", cat|less with language highlight
      pkgs.curl # curl duh
      pkgs.duf # nice disk usage output
      pkgs.fd # find util
      pkgs.file # file duh
      pkgs.fzf # fuzzy finder
      pkgs.jc # parse different formats and command outputs to json
      pkgs.jq # parse, format and query json documents
      pkgs.ncdu # disk usage navigator
      pkgs.pv # pipe viewer for progressbars in pipes
      pkgs.ripgrep # quick file searcher
      pkgs.testssl # print TLS certificate info
      pkgs.speedtest-cli # Speedtest command line util

      # Own tools:
      pkgs.nur.repos.etu.llr # llr, tool to cut long lines
      pkgs.nur.repos.etu.mkvcleaner # mkvcleaner, clean video files from unwanted tracks

      # Networking tools
      pkgs.dnsutils # dig etc
      pkgs.host # look up host info
      pkgs.whois # whois duh
      pkgs.prettyping # pretty ping output
      (pkgs.runCommand "prettyping-pp" {} ''
        mkdir -p $out/bin
        ln -s ${pkgs.prettyping}/bin/prettyping $out/bin/pp
      '')

      # Install some color test scripts from xterm
      (pkgs.runCommand "xterm-color-scripts" {} ''
        tar -xf ${pkgs.xterm.src}

        install -Dm755 xterm-${pkgs.xterm.version}/vttests/256colors2.pl $out/bin/256colors2.pl
        install -Dm755 xterm-${pkgs.xterm.version}/vttests/88colors2.pl $out/bin/88colors2.pl
      '')
    ];

    # Set backup file extensions for conflicts on home manager activation.
    home-manager.backupFileExtension = "backup";

    # Set state version for my users home-manager (if it's enabled).
    home-manager.users.${config.etu.user.username} = lib.mkIf config.etu.user.enable {
      home.stateVersion = config.etu.stateVersion;
    };

    # Set state version for root users home-manager.
    home-manager.users.root.home.stateVersion = config.etu.stateVersion;
  };
}
