let
  # Import my ssh public keys
  keys = (import ./data.nix).pubkeys;

  # Assemble public keys for user-facing computers
  etu = keys.etu.laptop-private-elis ++ keys.etu.laptop-work-elis;

  # Computers host keys
  hosts = let
    inherit (keys.systems) laptop-private-caroline laptop-private-elis laptop-work-elis server-main-elis server-sparv vps04 vps06;
  in {
    laptop-private-caroline = [laptop-private-caroline];
    laptop-private-elis = [laptop-private-elis];
    laptop-work-elis = [laptop-work-elis];
    server-main-elis = [server-main-elis];
    server-sparv = [server-sparv];
    vps04 = [vps04];
    vps06 = [vps06];

    all = [
      laptop-private-elis
      laptop-work-elis
      server-main-elis
      server-sparv
      vps04
      vps06
    ];
  };
in {
  "secrets/any/hashed-etu-password-file.age".publicKeys = etu ++ hosts.laptop-private-elis ++ hosts.laptop-work-elis ++ hosts.server-main-elis ++ hosts.vps04;
  "secrets/any/hashed-root-password-file.age".publicKeys = etu ++ hosts.all;
  "secrets/any/netdata-claim-token-file.age".publicKeys = etu ++ hosts.all;
  "secrets/laptop-private-caroline/hashed-concate-password-file.age".publicKeys = etu ++ hosts.laptop-private-caroline;
  "secrets/laptop-private-caroline/hashed-root-password-file.age".publicKeys = etu ++ hosts.laptop-private-caroline;
  "secrets/laptop-private-elis/etu_at_aarch64.nixos.community.age".publicKeys = etu ++ hosts.laptop-private-elis;
  "secrets/laptop-private-elis/etu_at_aarch64.nixos.community.pub.age".publicKeys = etu ++ hosts.laptop-private-elis;
  "secrets/server-main-elis/cloudflare-api-env.age".publicKeys = etu ++ hosts.server-main-elis;
  "secrets/server-main-elis/etu-freshrss-password.age".publicKeys = etu ++ hosts.server-main-elis;
  "secrets/server-main-elis/initrd-sshd-ec.age".publicKeys = etu ++ hosts.server-main-elis;
  "secrets/server-main-elis/nextcloud-admin-password.age".publicKeys = etu ++ hosts.server-main-elis;
  "secrets/server-main-elis/syncoid-ssh-ec.age".publicKeys = etu ++ hosts.server-main-elis;
  "secrets/server-main-elis/wallabag-secret.age".publicKeys = etu ++ hosts.server-main-elis;
  "secrets/server-sparv/valheim-server-env.age".publicKeys = etu ++ hosts.server-sparv;
  "secrets/vps04/flummbot.toml.age".publicKeys = etu ++ hosts.vps04;
  "secrets/vps04/hashed-ozeloten-password-file.age".publicKeys = etu ++ hosts.vps04;
  "secrets/vps06/matrix-sliding-sync-secret.age".publicKeys = etu ++ hosts.vps06;
  "secrets/workstations/syncoid-ssh-ec.age".publicKeys = etu ++ hosts.laptop-private-elis ++ hosts.laptop-work-elis;
}
