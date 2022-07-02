let
  # Import my ssh public keys
  keys = import ./data/pubkeys.nix;

  # Assemble public keys for user-facing computers
  etu = keys.etu.agrajag ++ keys.etu.work;

  # Computers host keys
  hosts = let
    inherit (keys.systems) agrajag fenchurch vps04 vps05 work;
  in {
    agrajag = [ agrajag.rsa agrajag.ec ];
    fenchurch = [ fenchurch.rsa fenchurch.ec ];
    vps04 = [ vps04.rsa vps04.ec ];
    vps05 = [ vps05.rsa vps05.ec ];
    work = [ work.rsa work.ec ];

    all = [
      agrajag.rsa agrajag.ec
      fenchurch.rsa fenchurch.ec
      vps05.rsa vps05.ec
      work.rsa work.ec
    ];
  };
in
{
  "secrets/agrajag/etu_at_aarch64.nixos.community.age".publicKeys = etu ++ hosts.agrajag;
  "secrets/agrajag/etu_at_aarch64.nixos.community.pub.age".publicKeys = etu ++ hosts.agrajag;
  "secrets/fenchurch/grafana-admin-password.age".publicKeys = etu ++ hosts.fenchurch;
  "secrets/vps04/flummbot.toml.age".publicKeys = etu ++ hosts.vps04;
  "secrets/vps05/halsobrev.se.key.age".publicKeys = etu ++ hosts.vps05;
  "secrets/vps05/halsobrev.se.pem.age".publicKeys = etu ++ hosts.vps05;
  "secrets/vps05/xn--hlsobrev-0za.se.key.age".publicKeys = etu ++ hosts.vps05;
  "secrets/vps05/xn--hlsobrev-0za.se.pem.age".publicKeys = etu ++ hosts.vps05;
  "secrets/workstations/nixos-data-secrets.nix.age".publicKeys = etu ++ hosts.agrajag ++ hosts.work;
}
