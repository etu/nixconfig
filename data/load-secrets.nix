if builtins.pathExists ./secrets.nix then import ./secrets.nix else {
  # Dummy passwords to use for accounts, remember to create a secrets.nix
  # with newly generated passwords using the following command:
  # $ nix-shell --run 'mkpasswd -m SHA-512 -s' -p mkpasswd
  hashedEtuPassword = "$6$iQ6X3IyRlMEF$z63d2c.i66RTiZHn7rD30gSsLAqfwjNxqa.EZxH0UJeQWyrjIELwniKO1MObq/P4alE1oaeOz8QmgIXP.BSXe1";
  hashedRootPassword = "$6$/SKTjvOf3cKY$3/qX1flzWray6Jtt2Br90ogiyYYLv6BeRabO3YJRpsNS3C/.lS544cPnByyRGtCVWtAPFc5kTuVz0nE/EFQY5/";
}
