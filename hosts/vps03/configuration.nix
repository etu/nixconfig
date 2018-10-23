# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

let
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

    # Import local modules & overlays
    ../../overlays/local/default.nix
  ];

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "17.09";

  networking.hostName = "vps03";

  # Set up bootloader
  boot.loader.grub.device = "/dev/vda";
  boot.cleanTmpDir = true;

  # Auto upgrade system
  system.autoUpgrade.enable = true;
  system.autoUpgrade.channel = "https://nixos.org/channels/nixos-18.09-small";

  # Auto garbage collect
  nix.gc.automatic = true;
  nix.gc.options = "--delete-older-than 14d";

  # Auto update the config before it upgrades the system
  my.update-config.enable = true;

  # Caddy
  services.caddy.enable = true;
  services.caddy.agree = true;
  services.caddy.email = "elis@hirwing.se";
  services.caddy.config = ''
    elis.nu, www.elis.nu {
      ${caddyTlsHsts}

      proxy / https://etu.github.io/ {
        header_upstream Host {host}
      }
    }

    sa.0b.se {
      ${caddyTlsHsts}

      proxy / https://etu.github.io/ {
        header_upstream Host elis.nu
      }
    }

    ix.ufs.se {
      ${caddyTlsHsts}

      proxy / https://ix-sthlm.github.io/ {
        header_upstream Host ix.ufs.se
      }
    }

    git.elis.nu {
      ${caddyTlsHsts}

      proxy / http://127.0.0.1:3000
    }

    https://ip.failar.nu {
      ${caddyTlsHsts}

      proxy / localhost:8123
    }

    http://ip.failar.nu {
      proxy / localhost:8123
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
  services.gitea.rootUrl = "https://git.elis.nu/";
  services.gitea.database.type = "postgres";
  services.gitea.database.passwordFile = "/var/lib/gitea-db-pass";
  services.gitea.extraConfig = ''
    [service]
    DISABLE_REGISTRATION = true
  '';

  # Postgres
  services.postgresql.package = pkgs.postgresql100;
  services.postgresql.dataDir = "/var/lib/postgresql/10.0";

  # Enable the ip-failar-nu service
  programs.ip-failar-nu.enable = true;

  # Enable sks keyserver
  services.sks.enable = true;
  services.sks.hkpAddress = [ "0.0.0.0" "::0" ];
  environment.systemPackages = with pkgs; [ pgpkeyserver-lite ];

  # Enable common cli settings for my systems
  my.common-cli.enable = true;

  # SSH Keys for remote logins
  users.extraUsers.root.openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILPvVYtcFHwuW/QW5Sqyuno7KrsVq9q9HUOBoaoIlIwu etu@hactar-2016-09-24"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPXaF1OwJAyGuPr3Rb0E+ut1gxVenll82/fLSc7p8UeA etu@fenchurch-2017-07-14"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINvIdD5t0Tjn+e41dIMt9VM5B0gs9yCuTY4p7Hpklrhr etu@ford-2018-03-05"
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQC30zMfq5/ZBnLPXiz4qtTsg3SU6voKQMumADNhTpVSKo3erANR5zTb1WPfjM4IWLCcWfksWDNOOeMaKM0hGgdnGbfrXpIOJwKNHaSp11cvQ6wTMAGV3B3ItJHOV+Czw4kEeUB+Tic8m+U2jnTPLXC4x3B7bdXHhdhmQbTpEq9pabe8eRQM54/9SuG6M9y8G3g35s3edsXrEnh/OI62a66F5aOugQH4fX5ehfGg3zk7LLu7U8bX9FGOeOVCCEBsfm5ysczNAO3v1iA4G9N8vgfAHJNZfKglYSQIi9nyURxcqT511OFTGK1cyWHGjqCNK286Plx90u0SVQvvG+9hkq2l/kbgmNpEYlTmAs22y+6j7R+gpSCMxJSfjXfJeyVcCTKo2CT9+SQRDz+pz+wyv/NgnaqXuP65RlwS0OIhdT6YheaFfbhkuMzFD78VDWOacLamWVQz/yTe5o+GhTavWVZyZ4Y9Wf+LB4sQtM9S5AqWpSCHDfA9nF6E9oWPbAZ4l+VD4vNQdrmByh+3uk+XP9/ytJVyJDd88MmxnC1yiN3xT7rQaUoUCmYy0Z7BLBYvBb2fU+JrJ8Eew6uf23SGYUtZuxanNANslBdvD0t68xCEHIxsVecpqRtbG8699bZEoWWbAFS1WO5EBAcYRfKV/4SQxLTO0J+HGQSRkgL7Ex1ioQ== etu@phouchg-4096-2016-04-18"
  ];
}
