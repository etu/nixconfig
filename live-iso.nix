#
# This definition is used to build a live-iso using my graphical environment.
#
# Build by running the following command:
# $ nix-build ./nix/nixos-unstable/nixos/ -I nixos-config=./live-iso.nix -A config.system.build.isoImage
#
# Then dd the resulting image:
# $ sudo dd if=result/iso/<tab> of=/dev/<device> status=progress bs=16M
#
{ lib, ... }:

{
  imports = [
    <nixpkgs/nixos/modules/installer/cd-dvd/installation-cd-base.nix>

    # Import my local modules
    ./modules
  ];

  # My module settings
  etu = {
    stateVersion = "22.11";
    graphical.enable = true;
    user.enable = true;
  };

  networking.wireless.enable = false;
  services.openssh.permitRootLogin = lib.mkForce "prohibit-password";
}
