# MacBook Air M5 managed with nix-darwin.
#
# Initial bootstrap (Nix must already be installed):
# 1. Move away the files nix-darwin wants to manage:
#    $ sudo mv /etc/nix/nix.conf /etc/nix/nix.conf.before-nix-darwin
# 2. Activate the configuration:
#    $ sudo nix --extra-experimental-features 'nix-command flakes' run nix-darwin -- switch --flake .#macbook-air-elis
#
# Later deployments:
# $ just activate
{
  inputs,
  lib,
  pkgs,
  ...
}:
{
  imports = [
    ./etu-options.nix
    inputs.nur.modules.darwin.default
  ];

  nixpkgs.hostPlatform = "aarch64-darwin";

  # Unfree packages allowed on this host, like etu.base.nix.allowUnfree on
  # the NixOS hosts. Also applies to home-manager (useGlobalPkgs).
  nixpkgs.config.allowUnfreePredicate =
    pkg:
    builtins.elem (lib.getName pkg) [
      "claude-code"
      "copilot-language-server" # emacs
      "intelephense" # emacs
    ];

  # Emacs overlay from nix community, used by the shared emacs home module
  # (modules/home/emacs) for emacsWithPackagesFromUsePackage.
  nixpkgs.overlays = [
    inputs.emacs-overlay.overlay
  ];

  # Fonts used by Emacs (and Ghostty can use them too).
  fonts.packages = [
    pkgs.emacs-all-the-icons-fonts
    pkgs.jetbrains-mono
  ];

  # Set the hostname so darwin-rebuild picks this configuration by default.
  networking.hostName = "macbook-air-elis";
  networking.computerName = "macbook-air-elis";
  networking.localHostName = "macbook-air-elis";

  # User that user-specific options (defaults, homebrew, etc) apply to.
  system.primaryUser = "elishirwing";
  users.users.elishirwing.home = "/Users/elishirwing";

  # Nix settings, mirroring modules/nixos/base on the NixOS hosts.
  nix.settings.experimental-features = [
    "nix-command"
    "flakes"
  ];
  nix.settings.trusted-users = [
    "root"
    "elishirwing"
  ];
  nix.settings.substituters = [
    "https://nix-community.cachix.org"
    "https://etu.cachix.org"
  ];
  nix.settings.trusted-public-keys = [
    "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    "etu.cachix.org-1:CeyfbMJZHZ95TScp8+I8+EeyzbncqPSj1xfCK9vOAFE="
  ];

  # Remap Caps Lock to Control.
  system.keyboard.enableKeyMapping = true;
  system.keyboard.remapCapsLockToControl = true;

  # Install fish system wide, load the Nix environment in it and add it to
  # /etc/shells. nix-darwin only changes login shells of users listed in
  # users.knownUsers, so switch to it once with:
  # $ chsh -s /run/current-system/sw/bin/fish
  programs.fish.enable = true;
  environment.shells = [ pkgs.fish ];

  # Use zoxide for cd in fish, like on laptop-private-elis.
  etu.base.fish.enableUserZoxideCd = true;

  # Tailscale, running the open source tailscaled as a launchd daemon (no menu
  # bar app). Log in once after activation with:
  # $ sudo tailscale up
  services.tailscale.enable = true;

  # Use Touch ID for sudo.
  security.pam.services.sudo_local.touchIdAuth = true;

  environment.systemPackages = [
    pkgs.just
    pkgs.nix-output-monitor

    # Command line tools, mirroring modules/nixos/base/packages on the NixOS
    # hosts (minus the Linux-only ones).
    pkgs.bat # "bat - cat with wings", cat|less with language highlight
    pkgs.btop # CLI system monitor
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
    (pkgs.runCommand "prettyping-pp" { } ''
      mkdir -p $out/bin
      ln -s ${pkgs.prettyping}/bin/prettyping $out/bin/pp
    '')

    # Install some color test scripts from xterm
    (pkgs.runCommand "xterm-color-scripts" { } ''
      tar -xf ${pkgs.xterm.src}

      install -Dm755 xterm-${pkgs.xterm.version}/vttests/256colors2.pl $out/bin/256colors2.pl
      install -Dm755 xterm-${pkgs.xterm.version}/vttests/88colors2.pl $out/bin/88colors2.pl
    '')
  ];

  # Used for backwards compatibility, read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 6;
}
