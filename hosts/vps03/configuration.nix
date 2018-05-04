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
    ../../profiles/common-server.nix
  ];

  networking.hostName = "vps03";

  # Set up bootloader
  boot.loader.grub.device = "/dev/vda";
  boot.cleanTmpDir = true;

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
  '';

  # Firewall
  networking.firewall.allowedTCPPorts = [ 80 443 ];

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
}
