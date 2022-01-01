let
  # Import my ssh public keys
  keys = import ../data/pubkeys.nix;

  # Assemble public keys for user-facing computers
  etu = keys.etu.agrajag ++ keys.etu.work;

  # Computers host keys
  hosts = let
    inherit (keys.systems) agrajag fenchurch vps05 work;
  in {
    agrajag = [ agrajag.rsa agrajag.ec ];
    fenchurch = [ fenchurch.rsa fenchurch.ec ];
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
  "nixos-data-secrets.nix.age".publicKeys = etu ++ hosts.agrajag ++ hosts.work;
  "sshkeys/etu_at_aarch64.nixos.community.pub.age".publicKeys = etu ++ hosts.agrajag;
  "sshkeys/etu_at_aarch64.nixos.community.age".publicKeys = etu ++ hosts.agrajag;
  "monitoring/grafana-admin-password.age".publicKeys = etu ++ hosts.fenchurch;
  "certs/xn--hlsobrev-0za.se.pem.age".publicKeys = etu ++ hosts.vps05;
  "certs/xn--hlsobrev-0za.se.key.age".publicKeys = etu ++ hosts.vps05;
  "certs/halsobrev.se.pem.age".publicKeys = etu ++ hosts.vps05;
  "certs/halsobrev.se.key.age".publicKeys = etu ++ hosts.vps05;
}
