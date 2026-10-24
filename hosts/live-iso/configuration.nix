#
# This definition is used to build a live-iso with my base setup.
#
# Build by running the following command:
# $ nix build .#nixosConfigurations.live-iso.config.system.build.isoImage
#
# Then dd the resulting image:
# $ sudo dd if=result/iso/<tab> of=/dev/<device> status=progress bs=16M
#
{
  lib,
  flake,
  modulesPath,
  ...
}:
{
  imports = [
    # Import base settings for live isos
    (modulesPath + "/installer/cd-dvd/installation-cd-base.nix")

    # Import my default modules
    flake.nixosModules.default
  ];

  # My module settings
  etu = {
    stateVersion = "26.05";

    # This is to make the openssh identity files to be located in a
    # reasonable place.
    dataPrefix = "/";

    # Enable my user account.
    user.enable = true;

    # Don't set a password for root / user depending on agenix.
    user.setEmptyPassword = true;
    user.setEmptyRootPassword = true;

    # Force disable persistence modules since this system doesn't
    # use ZFS.
    base.zfs.enable = false;

    # Force disable sanoid modules since this system doesn't use ZFS.
    base.sanoid.enable = false;

    # There's no need to run tailscale on the live-iso.
    base.tailscale.enable = false;
  };

  services.openssh.settings.PermitRootLogin = lib.mkForce "prohibit-password";

  # Use the host platform for building by default to avoid cross compiling.
  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
}
