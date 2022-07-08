{ config, lib, ... }:

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
  };
}
