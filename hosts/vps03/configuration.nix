{ pkgs, ... }: {
  imports = [
    ./hardware-configuration.nix
    ./networking.nix
    ../../profiles/common.nix
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
      tls {
        protocols tls1.2
        key_type p384
      }

      header / {
        Strict-Transport-Security max-age=31536000
      }

      proxy / https://etu.github.io/ {
        header_upstream Host {host}
      }
    }

    sa.0b.se {
      tls {
        protocols tls1.2
        key_type p384
      }

      header / {
        Strict-Transport-Security max-age=31536000
      }

      proxy / https://etu.github.io/ {
        header_upstream Host elis.nu
      }
    }

    ix.ufs.se {
      tls {
        protocols tls1.2
        key_type p384
      }

      header / {
        Strict-Transport-Security max-age=31536000
      }

      proxy / https://ix-sthlm.github.io/ {
        header_upstream Host ix.ufs.se
      }
    }

    git.elis.nu {
      tls {
        protocols tls1.2
        key_type p384
      }

      proxy / http://127.0.0.1:3000
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
    [log]
    ROOT_PATH = /var/lib/gitea
  '';

  # Enable default shell so we can use git over ssh
  users.extraUsers.gitea.useDefaultShell = true;

  # Postgres
  services.postgresql.package = pkgs.postgresql100;
  services.postgresql.dataDir = "/var/lib/postgresql/10.0";

  # Install the ip.failar.nu program
  environment.systemPackages = with pkgs; [
    (callPackage ../../packages/ip-failar-nu.nix {})
  ];
}
