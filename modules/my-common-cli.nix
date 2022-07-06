{ config, lib, pkgs, ... }:
let
  cfg = config.my.common-cli;

in
{
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

    # Enable mosh.
    programs.mosh.enable = true;

    # Enable firewall.
    networking.firewall.enable = true;
    networking.firewall.allowPing = true;

    # Enable lorri on all systems.
    services.lorri.enable = true;

    # Enable doas on all systems.
    security.doas.enable = true;

    # List packages installed in system profile. To search by name, run:
    # $ nix-env -qaP | grep wget
    environment.systemPackages = with pkgs; [
      bat
      bc # Dependency for some fish functions
      comma # The "," command which allows to run non-installed things ", htop"
      curl
      duf
      fd
      file
      fzf
      git
      host
      ncdu
      nix-top
      pv
      ripgrep
      tmux
      whois

      # Install prettyping
      prettyping

      # With pp shortcut
      (pkgs.runCommandNoCC "prettyping-pp" { } ''
        mkdir -p $out/bin
        ln -s ${pkgs.prettyping}/bin/prettyping $out/bin/pp
      '')

      # Install some color test scripts from xterm
      (pkgs.runCommandNoCC "xterm-color-scripts" { } ''
        tar -xf ${pkgs.xterm.src}

        install -Dm755 xterm-${pkgs.xterm.version}/vttests/256colors2.pl $out/bin/256colors2.pl
        install -Dm755 xterm-${pkgs.xterm.version}/vttests/88colors2.pl $out/bin/88colors2.pl
      '')
    ];
  };
}
