{ config, lib, ... }:
let
  # Import my ssh public keys
  keys = (import ../../../data.nix).pubkeys;

in
{
  options.etu.base.sshd.enable = lib.mkEnableOption "Enable base sshd settings";

  config = lib.mkIf config.etu.base.sshd.enable {
    # Enable the OpenSSH daemon.
    services.openssh.enable = true;

    # Default is true, let's disable password auth by default.
    services.openssh.passwordAuthentication = false;

    # This is default, but nice to make it clear in here.
    services.openssh.permitRootLogin = "prohibit-password";

    # Enable mosh.
    programs.mosh.enable = true;

    # Override identity paths for agenix since the openssh default paths
    # relies on a symlink being created in /etc/ssh to point at the
    # right path to make it to work as it would be in the right place.
    age.identityPaths = [
      "/persistent/etc/ssh/ssh_host_ed25519_key"
      "/persistent/etc/ssh/ssh_host_rsa_key"
    ];

    # Persistence of ssh key files
    environment.persistence."/persistent".files = [
      "/etc/ssh/ssh_host_rsa_key"
      "/etc/ssh/ssh_host_rsa_key.pub"
      "/etc/ssh/ssh_host_ed25519_key"
      "/etc/ssh/ssh_host_ed25519_key.pub"
    ];

    # Add known hosts for all of my systems that I access remotely to
    # they always are trusted.
    programs.ssh.knownHosts = {
      fenchurch-ec = {
        extraHostNames = [ "home.elis.nu" "local.elis.nu" "192.168.1.101" ];
        publicKey = keys.systems.fenchurch.ec;
      };
      fenchurch-rsa = {
        extraHostNames = [ "home.elis.nu" "local.elis.nu" "192.168.1.101" ];
        publicKey = keys.systems.fenchurch.rsa;
      };
      vps04-ec = {
        extraHostNames = [ "vps04.elis.nu" ];
        publicKey = keys.systems.vps04.ec;
      };
      vps04-rsa = {
        extraHostNames = [ "vps04.elis.nu" ];
        publicKey = keys.systems.vps04.rsa;
      };
      vps05-ec = {
        extraHostNames = [ "git.elis.nu" "vps05.elis.nu" ];
        publicKey = keys.systems.vps05.ec;
      };
      vps05-rsa = {
        extraHostNames = [ "git.elis.nu" "vps05.elis.nu" ];
        publicKey = keys.systems.vps05.rsa;
      };
    };
  };
}
