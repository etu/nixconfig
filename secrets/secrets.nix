let
  # Import my ssh public keys
  keys = import ../data/pubkeys.nix;

  # Assemble public keys for user-facing computers
  etu = keys.etu.agrajag ++ keys.etu.work;

  # Computers host keys
  hosts = let
    inherit (keys.systems) fenchurch;
  in {
    fenchurch = [ fenchurch.rsa fenchurch.ec ];

    all = [
      fenchurch.rsa fenchurch.ec
    ];
  };
in
{
  "nagios-elis-nu.age".publicKeys = etu ++ hosts.fenchurch;
}
