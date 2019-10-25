# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

let
  # Import my ssh public keys
  keys = import ../../data/pubkeys.nix;

  # Caddy template
  caddyTlsHsts = ''
      tls {
        protocols tls1.2
        key_type p384
      }

      header / {
        Strict-Transport-Security max-age=31536000
      }
  '';

in {
  imports = [
    ./hardware-configuration.nix
    ./networking.nix
    ./persistence.nix

    # Import local modules
    ../../modules
  ];

  # Use the GRUB 2 boot loader.
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.device = "/dev/sda";

  networking.hostName = "vps05";

  # Auto upgrade system
  system.autoUpgrade.enable = true;
  system.autoUpgrade.channel = "https://nixos.org/channels/nixos-19.03-small";

  # Auto garbage collect
  nix.gc.automatic = true;
  nix.gc.options = "--delete-older-than 14d";

  # Auto update the config before it upgrades the system
  my.update-config.enable = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    git
    htop
  ];

  # Install mosh
  programs.mosh.enable = true;

  # Install fish
  programs.fish.enable = true;

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Caddy
  services.caddy.enable = true;
  services.caddy.agree = true;
  services.caddy.email = "elis@hirwing.se";
  services.caddy.config = ''
    git.elis.nu {
      ${caddyTlsHsts}

      proxy / http://127.0.0.1:3000
    }

    sa.0b.se {
      ${caddyTlsHsts}

      proxy / https://elis.nu/
    }

    https://ip.failar.nu {
      ${caddyTlsHsts}

      proxy / http://127.0.0.1:8123/
    }

    http://ip.failar.nu {
      proxy / http://127.0.0.1:8123/
    }

    # Set up webserver for pgpkeyserver-lite and routes for sks
    keys.ix.ufs.se {
      ${caddyTlsHsts}

      root ${pkgs.pgpkeyserver-lite}
    }
    keys.ix.ufs.se/pks {
      ${caddyTlsHsts}

      proxy / http://127.0.0.1:11371/pks
    }
  '';

  # Open Firewall for HTTP, HTTPS and hkp (keyserver)
  networking.firewall.allowedTCPPorts = [ 80 443 11371 ];

  # Gitea dump
  services.gitea.dump.enable = true;

  # Gitea
  services.gitea.enable = true;
  services.gitea.appName = "Elis Git Service";
  services.gitea.cookieSecure = true;
  services.gitea.domain = "git.elis.nu";
  services.gitea.rootUrl = "https://git.elis.nu/";
  services.gitea.database.type = "postgres";
  services.gitea.database.passwordFile = "/nix/persistent/var/lib/gitea-db-pass";
  services.gitea.disableRegistration = true;

  # Postgres
  services.postgresql.enable = true;
  services.postgresql.package = pkgs.postgresql_11;

  # Enable the ip-failar-nu service
  programs.ip-failar-nu.enable = true;

  # Enable sks keyserver
  services.sks.enable = true;
  services.sks.hkpAddress = [ "0.0.0.0" "::0" ];
  services.sks.extraDbConfig = "set_flags               DB_LOG_AUTOREMOVE";

  # Enable common cli settings for my systems
  my.common-cli.enable = true;

  # Set up users accounts:
  users.mutableUsers = false;

  users.users.root.openssh.authorizedKeys.keys = with keys.etu; hactar ++ agrajag ++ ford-x250 ++ work;

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "19.03"; # Did you read the comment?
}
