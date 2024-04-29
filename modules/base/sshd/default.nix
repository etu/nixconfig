{
  config,
  lib,
  myData,
  ...
}: {
  options.etu.base.sshd.enable = lib.mkEnableOption "Enable base sshd settings";

  config = lib.mkIf config.etu.base.sshd.enable {
    # Enable the OpenSSH daemon.
    services.openssh.enable = true;

    # Default is true, let's disable password auth by default.
    services.openssh.settings.PasswordAuthentication = false;

    # This is default, but nice to make it clear in here.
    services.openssh.settings.PermitRootLogin = "prohibit-password";

    # Enable mosh.
    programs.mosh.enable = true;

    # Override identity paths for agenix since the openssh default paths
    # relies on a symlink being created in /etc/ssh to point at the
    # right path to make it to work as it would be in the right place.
    age.identityPaths = [
      "${config.etu.dataPrefix}/etc/ssh/ssh_host_ed25519_key"
      "${config.etu.dataPrefix}/etc/ssh/ssh_host_rsa_key"
    ];

    # Persistence of ssh key files
    etu.base.zfs.system.files = [
      "/etc/ssh/ssh_host_rsa_key"
      "/etc/ssh/ssh_host_rsa_key.pub"
      "/etc/ssh/ssh_host_ed25519_key"
      "/etc/ssh/ssh_host_ed25519_key.pub"
    ];

    # Add known hosts for all of my systems that I access remotely to
    # they always are trusted.
    programs.ssh.knownHosts = {
      laptop-private-caroline.publicKey = myData.pubkeys.systems.laptop-private-caroline;
      laptop-private-elis.publicKey = myData.pubkeys.systems.laptop-private-elis;
      laptop-work-elis.publicKey = myData.pubkeys.systems.laptop-work-elis;
      server-main-elis = {
        extraHostNames = ["home.elis.nu" "local.elis.nu" "192.168.1.101"];
        publicKey = myData.pubkeys.systems.server-main-elis;
      };
      server-main-elis-initrd = {
        extraHostNames = ["home.elis.nu" "local.elis.nu" "192.168.1.101"];
        publicKey = myData.pubkeys.systems.server-main-elis-initrd;
      };
      "sparv.failar.nu".publicKey = myData.pubkeys.systems.server-sparv;
      "vps06.elis.nu" = {
        extraHostNames = ["git.elis.nu"];
        publicKey = myData.pubkeys.systems.vps06;
      };
    };
  };
}
