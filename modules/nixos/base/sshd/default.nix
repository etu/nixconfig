{
  config,
  lib,
  ...
}:
{
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

    # Add known hosts for all of my systems that I access remotely so
    # they are always trusted.
    programs.ssh.knownHosts = {
      server-main-elis = {
        extraHostNames = [
          "192.168.1.10"
        ];
        publicKey = config.etu.data.pubkeys.systems.server-main-elis;
      };
      server-main-elis-initrd = {
        extraHostNames = [
          "192.168.1.10"
          # The initrd's own tailnet node, sshd listens on 2222
          "[server-main-elis-initrd]:2222"
        ];
        publicKey = config.etu.data.pubkeys.systems.server-main-elis-initrd;
      };
      "vps06.elis.nu" = {
        extraHostNames = [
          "vps06"
          "git.elis.nu"
        ];
        publicKey = config.etu.data.pubkeys.systems.vps06;
      };
    };
  };
}
