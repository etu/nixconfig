let
  # Import my ssh public keys
  keys = (import ./data.nix).pubkeys;

  # Assemble public keys for user-facing computers
  etu = keys.etu.agrajag ++ keys.etu.work;

  # Computers host keys
  hosts = let
    inherit (keys.systems) agrajag fenchurch vps04 vps06 work;
  in {
    agrajag = [ agrajag.rsa agrajag.ec ];
    fenchurch = [ fenchurch.rsa fenchurch.ec ];
    vps04 = [ vps04.rsa vps04.ec ];
    vps06 = [ vps06.rsa vps06.ec ];
    work = [ work.rsa work.ec ];

    all = [
      agrajag.rsa agrajag.ec
      fenchurch.rsa fenchurch.ec
      vps04.rsa vps04.ec
      vps06.rsa vps06.ec
      work.rsa work.ec
    ];
  };
in
{
  "secrets/any/hashed-etu-password-file.age".publicKeys = etu ++ hosts.agrajag ++ hosts.work ++ hosts.fenchurch ++ hosts.vps04;
  "secrets/any/hashed-root-password-file.age".publicKeys = etu ++ hosts.all;
  "secrets/agrajag/etu_at_aarch64.nixos.community.age".publicKeys = etu ++ hosts.agrajag;
  "secrets/agrajag/etu_at_aarch64.nixos.community.pub.age".publicKeys = etu ++ hosts.agrajag;
  "secrets/fenchurch/cloudflare-api-env.age".publicKeys = etu ++ hosts.fenchurch;
  "secrets/fenchurch/etu-freshrss-password.age".publicKeys = etu ++ hosts.fenchurch;
  "secrets/fenchurch/grafana-admin-password.age".publicKeys = etu ++ hosts.fenchurch;
  "secrets/fenchurch/initrd-sshd-ec.age".publicKeys = etu ++ hosts.fenchurch;
  "secrets/fenchurch/initrd-sshd-rsa.age".publicKeys = etu ++ hosts.fenchurch;
  "secrets/fenchurch/syncoid-ssh-ec.age".publicKeys = etu ++ hosts.fenchurch;
  "secrets/vps04/flummbot.toml.age".publicKeys = etu ++ hosts.vps04;
  "secrets/vps04/hashed-ozeloten-password-file.age".publicKeys = etu ++ hosts.vps04;
  "secrets/workstations/nixos-data-secrets.nix.age".publicKeys = etu ++ hosts.agrajag ++ hosts.work;
  "secrets/workstations/syncoid-ssh-ec.age".publicKeys = etu ++ hosts.agrajag ++ hosts.work;
}
