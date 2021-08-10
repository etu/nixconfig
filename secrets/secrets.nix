let
  # Import my ssh public keys
  keys = import ../data/pubkeys.nix;

  # Assemble public keys for user-facing computers
  etu = keys.etu.agrajag ++ keys.etu.work;

  # Computers host keys
  hosts = let
    inherit (keys.systems) agrajag fenchurch;
  in {
    agrajag = [ agrajag.rsa agrajag.ec ];
    fenchurch = [ fenchurch.rsa fenchurch.ec ];

    all = [
      agrajag.rsa agrajag.ec
      fenchurch.rsa fenchurch.ec
    ];
  };
in
{
  "nagios-elis-nu.age".publicKeys = etu ++ hosts.fenchurch;
  "sshkeys/etu_at_aarch64.nixos.community.pub.age".publicKeys = etu ++ hosts.agrajag;
  "sshkeys/etu_at_aarch64.nixos.community.age".publicKeys = etu ++ hosts.agrajag;
}
