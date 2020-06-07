{ config, lib, pkgs, ... }:

let
  cfg = config.my.common-cli;

in
{
  options.my.common-cli.enable = lib.mkEnableOption "Enables my common CLI thingys";

  config = lib.mkIf cfg.enable {
    # Set your time zone.
    time.timeZone = "Europe/Stockholm";

    # Select internationalisation properties.
    i18n = {
      defaultLocale = "en_US.UTF-8";
      supportedLocales = [
        "all"
      ];
    };

    console.font = "Lat2-Terminus16";
    console.keyMap = "dvorak";

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

    # Enable lorri on all systems.
    services.lorri.enable = true;

    # Root shell
    users.extraUsers.root.shell = pkgs.fish;

    # List packages installed in system profile. To search by name, run:
    # $ nix-env -qaP | grep wget
    environment.systemPackages = with pkgs; [
      bat
      bc # Dependency for some fish functions
      curl
      fd
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
