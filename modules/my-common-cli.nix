{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.my.common-cli;

in {
  options.my.common-cli.enable = mkEnableOption "Enables my common CLI thingys";

  config = mkIf cfg.enable {
    # Set your time zone.
    time.timeZone = "Europe/Stockholm";

    # Select internationalisation properties.
    i18n = {
      consoleFont = "Lat2-Terminus16";
      consoleKeyMap = "dvorak-sv-a1";
      defaultLocale = "en_US.UTF-8";
      supportedLocales = [
        "all"
      ];
    };

    # Enable the OpenSSH daemon.
    services.openssh.enable = true;
    services.openssh.passwordAuthentication = false;

    # Enable fish.
    programs.fish.enable = true;

    # Enable mosh.
    programs.mosh.enable = true;

    # Enable firewall.
    networking.firewall.enable = true;
    networking.firewall.allowPing = true;

    # Root shell
    users.extraUsers.root.shell = pkgs.fish;

    # List packages installed in system profile. To search by name, run:
    # $ nix-env -qaP | grep wget
    environment.systemPackages = with pkgs; [
      bat
      bc                # Dependency for some fish functions
      curl
      file
      fzf
      git
      host
      htop
      ncdu
      pv
      ripgrep
      tmux
      whois
    ];
  };
}
