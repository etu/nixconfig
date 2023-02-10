#
# This definition is used to build a live-iso using my graphical environment.
#
# Build by running the following command:
# $ nix build .#nixosConfigurations.live-iso.config.system.build.isoImage
#
# Then dd the resulting image:
# $ sudo dd if=result/iso/<tab> of=/dev/<device> status=progress bs=16M
#
{
  config,
  lib,
  modulesPath,
  ...
}: {
  imports = [
    # Import base settings for live isos
    (modulesPath + "/installer/cd-dvd/installation-cd-base.nix")
  ];

  # My module settings
  etu = {
    stateVersion = "22.11";

    # This is to make the openssh identity files to be located in a
    # reasonable place.
    dataPrefix = "/";

    # Enable my user account.
    user.enable = true;

    # Enable a graphical system.
    graphical.enable = true;

    # Force disable persistence modules since this system doesn't
    # use ZFS.
    base.zfs.enable = lib.mkForce false;

    # Force disable sanoid modules since this system doesn't use ZFS.
    base.sanoid.enable = lib.mkForce false;
  };

  # Force override root users password to empty string.
  users.users.root = {
    passwordFile = lib.mkForce null;
    initialPassword = "";
  };

  # Force override my users password to empty string.
  users.users.${config.etu.user.username} = {
    passwordFile = lib.mkForce null;
    initialPassword = "";
  };

  networking.wireless.enable = false;
  services.openssh.settings.PermitRootLogin = lib.mkForce "prohibit-password";
}
