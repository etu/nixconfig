let
  # Import my ssh public keys
  keys = (import ./data.nix).pubkeys;

  # Assemble public keys for user-facing computers
  etu = keys.etu.desktop-elis ++ keys.etu.laptop-private-elis ++ keys.etu.laptop-work-elis;

  # Computers host keys
  hosts = let
    inherit (keys.systems) desktop-elis laptop-private-caroline laptop-private-elis laptop-work-elis server-main-elis server-sparv vps06;
  in {
    desktop-elis = [desktop-elis];
    laptop-private-caroline = [laptop-private-caroline];
    laptop-private-elis = [laptop-private-elis];
    laptop-work-elis = [laptop-work-elis];
    server-main-elis = [server-main-elis];
    server-sparv = [server-sparv];
    vps06 = [vps06];

    all = [
      desktop-elis
      laptop-private-elis
      laptop-work-elis
      server-main-elis
      server-sparv
      vps06
    ];
  };
in {
  "secrets/any/hashed-etu-password-file.age".publicKeys = etu ++ hosts.desktop-elis ++ hosts.laptop-private-elis ++ hosts.laptop-work-elis ++ hosts.server-main-elis;
  "secrets/any/hashed-root-password-file.age".publicKeys = etu ++ hosts.all;
  "secrets/any/netdata-claim-token-file.age".publicKeys = etu ++ hosts.all;
  "secrets/laptop-private-caroline/hashed-concate-password-file.age".publicKeys = etu ++ hosts.laptop-private-caroline;
  "secrets/laptop-private-caroline/hashed-root-password-file.age".publicKeys = etu ++ hosts.laptop-private-caroline;
  "secrets/laptop-private-elis/etu_at_aarch64.nixos.community.age".publicKeys = etu ++ hosts.desktop-elis ++ hosts.laptop-private-elis;
  "secrets/laptop-private-elis/etu_at_aarch64.nixos.community.pub.age".publicKeys = etu ++ hosts.desktop-elis ++ hosts.laptop-private-elis;
  "secrets/server-main-elis/cloudflare-api-env.age".publicKeys = etu ++ hosts.server-main-elis;
  "secrets/server-main-elis/etu-freshrss-password.age".publicKeys = etu ++ hosts.server-main-elis;
  "secrets/server-main-elis/homepage-dashboard-environment.age".publicKeys = etu ++ hosts.server-main-elis;
  "secrets/server-main-elis/initrd-sshd-ec.age".publicKeys = etu ++ hosts.server-main-elis;
  "secrets/server-main-elis/nextcloud-admin-password.age".publicKeys = etu ++ hosts.server-main-elis;
  "secrets/server-main-elis/syncoid-ssh-ec.age".publicKeys = etu ++ hosts.server-main-elis;
  "secrets/server-sparv/valheim-server-env.age".publicKeys = etu ++ hosts.server-sparv;
  "secrets/workstations/syncoid-ssh-ec.age".publicKeys = etu ++ hosts.desktop-elis ++ hosts.laptop-private-elis ++ hosts.laptop-work-elis;
}
