# Single source of truth for all agenix secrets.
#
# Each attribute defines one secret. The fields map directly to the agenix
# NixOS module options (file, owner, group, mode, path, symlink), plus one
# extra field consumed only here:
#
#   hostKeys – list of SSH public-key strings (beyond the etu user keys)
#              that are allowed to decrypt this secret.  Used to generate
#              secrets.nix for the agenix CLI tool.
#
# To add a new secret:
#   1. agenix -e secrets/<dir>/name.age
#   2. Add an entry here with the correct file path and hostKeys.
#   3. git add the .age file.
#   Done – secrets.nix and data.nix ageModules are derived automatically.
let
  keys = import ./pubkeys.nix;

  # etu's personal computer user keys – always allowed to decrypt every secret.
  etu = keys.etu.desktop-elis ++ keys.etu.laptop-private-elis ++ keys.etu.laptop-work-elis;

  # Host system key shorthands (each is a single string so we wrap in a list).
  sys = keys.systems;
  h = {
    desktop-caroline = [ sys.desktop-caroline ];
    desktop-elis = [ sys.desktop-elis ];
    laptop-private-caroline = [ sys.laptop-private-caroline ];
    laptop-private-elis = [ sys.laptop-private-elis ];
    laptop-work-elis = [ sys.laptop-work-elis ];
    server-main-elis = [ sys.server-main-elis ];
    server-sparv = [ sys.server-sparv ];
    vps06 = [ sys.vps06 ];

    # Convenience groups
    all = [
      sys.desktop-elis
      sys.laptop-private-elis
      sys.laptop-work-elis
      sys.server-main-elis
      sys.server-sparv
      sys.vps06
    ];
    workstations = [
      sys.desktop-elis
      sys.laptop-private-elis
      sys.laptop-work-elis
    ];
  };
in
{
  # ---------------------------------------------------------------------------
  # Shared / any-host secrets
  # ---------------------------------------------------------------------------

  hashed-etu-password = {
    file = ./secrets/any/hashed-etu-password-file.age;
    hostKeys =
      etu ++ h.desktop-elis ++ h.laptop-private-elis ++ h.laptop-work-elis ++ h.server-main-elis;
  };

  hashed-root-password = {
    file = ./secrets/any/hashed-root-password-file.age;
    hostKeys = etu ++ h.all;
  };

  # ---------------------------------------------------------------------------
  # Caroline's laptop secrets
  # ---------------------------------------------------------------------------

  hashed-caroline-laptop-concate-password = {
    file = ./secrets/laptop-private-caroline/hashed-concate-password-file.age;
    hostKeys = etu ++ h.laptop-private-caroline ++ h.desktop-caroline;
  };

  hashed-caroline-laptop-root-password = {
    file = ./secrets/laptop-private-caroline/hashed-root-password-file.age;
    hostKeys = etu ++ h.laptop-private-caroline ++ h.desktop-caroline;
  };

  # ---------------------------------------------------------------------------
  # server-main-elis secrets
  # ---------------------------------------------------------------------------

  server-main-elis-initrd-sshd = {
    file = ./secrets/server-main-elis/initrd-sshd-ec.age;
    path = "/data/etc/initrd-ssh/ssh_host_ed_25519_key";
    symlink = false;
    hostKeys = etu ++ h.server-main-elis;
  };

  cloudflare-api-env = {
    file = ./secrets/server-main-elis/cloudflare-api-env.age;
    hostKeys = etu ++ h.server-main-elis;
  };

  freshrss-password-etu = {
    file = ./secrets/server-main-elis/etu-freshrss-password.age;
    owner = "freshrss";
    hostKeys = etu ++ h.server-main-elis;
  };

  homepage-dashboard-environment = {
    file = ./secrets/server-main-elis/homepage-dashboard-environment.age;
    hostKeys = etu ++ h.server-main-elis;
  };

  nextcloud-admin-password = {
    file = ./secrets/server-main-elis/nextcloud-admin-password.age;
    owner = "nextcloud";
    hostKeys = etu ++ h.server-main-elis;
  };

  beszel-ssh-ec = {
    file = ./secrets/server-main-elis/beszel-ssh-ec.age;
    path = "/data/var/lib/beszel-hub/beszel_data/id_ed25519";
    owner = "root";
    hostKeys = etu ++ h.server-main-elis;
  };

  syncoid-server-main-elis-ssh-ec = {
    file = ./secrets/server-main-elis/syncoid-ssh-ec.age;
    path = "/var/lib/syncoid/.ssh/id_ed25519";
    owner = "syncoid";
    symlink = false;
    hostKeys = etu ++ h.server-main-elis;
  };

  # ---------------------------------------------------------------------------
  # vps06 secrets
  # ---------------------------------------------------------------------------

  grafana-secret-key = {
    file = ./secrets/vps06/grafana-secret-key.age;
    owner = "grafana";
    hostKeys = etu ++ h.vps06;
  };

  # ---------------------------------------------------------------------------
  # server-sparv secrets
  # ---------------------------------------------------------------------------

  valheim-server-env = {
    file = ./secrets/server-sparv/valheim-server-env.age;
    hostKeys = etu ++ h.server-sparv;
  };

  project-zomboid-env = {
    file = ./secrets/server-sparv/project-zomboid-env.age;
    hostKeys = etu ++ h.server-sparv;
  };

  enshrouded-server-env = {
    file = ./secrets/server-sparv/enshrouded-server-env.age;
    hostKeys = etu ++ h.server-sparv;
  };

  # ---------------------------------------------------------------------------
  # Workstation secrets
  # ---------------------------------------------------------------------------

  syncoid-workstations-ssh-ec = {
    file = ./secrets/workstations/syncoid-ssh-ec.age;
    path = "/var/lib/syncoid/.ssh/id_ed25519";
    owner = "syncoid";
    symlink = false;
    hostKeys = etu ++ h.workstations;
  };
}
