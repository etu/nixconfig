{ ... }: {
  imports = [
    ./hardware-configuration.nix
    ./networking.nix
    ../../profiles/common.nix
  ];

  networking.hostName = "vps03";

  # Set up bootloader
  boot.loader.grub.device = "/dev/vda";
  boot.cleanTmpDir = true;
}
