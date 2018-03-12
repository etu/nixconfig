{ ... }:
{
  imports = [ <nixpkgs/nixos/modules/profiles/qemu-guest.nix> ];
  fileSystems."/" = { device = "/dev/vda1"; fsType = "ext4"; };
}
