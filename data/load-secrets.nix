let
  # If we have a locally defined secrets file, let's import it to
  # overwrite the dummy default secrets.
  realHashes = if builtins.pathExists ./secrets.nix then import ./secrets.nix else {};
in {
  # Dummy passwords to use for accounts, remember to create a secrets.nix
  # with newly generated passwords using the following command:
  # $ nix-shell --run 'mkpasswd -m SHA-512 -s' -p mkpasswd
  hashedEtuPassword = "$6$iQ6X3IyRlMEF$z63d2c.i66RTiZHn7rD30gSsLAqfwjNxqa.EZxH0UJeQWyrjIELwniKO1MObq/P4alE1oaeOz8QmgIXP.BSXe1";
  hashedOzelotenPassword = "$6$Tp21Uo367npe/$/a6taUbyYu3QQo8RAPp6krKAq8wNs67hqSc0KPCzT32N7Aqkud110qddUAywOGKdYPJc/23BqogmUpVBQEoGF/";
  hashedRootPassword = "$6$/SKTjvOf3cKY$3/qX1flzWray6Jtt2Br90ogiyYYLv6BeRabO3YJRpsNS3C/.lS544cPnByyRGtCVWtAPFc5kTuVz0nE/EFQY5/";
} // realHashes
