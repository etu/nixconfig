#
# This is my build of mmdvmhost for nixos as a raspberry pi image.
#
# Currently I cross complie this by running:
# $ nix-build "<nixpkgs/nixos>" -I nixos-config=./hosts/nix-star/sdimage.nix -A config.system.build.sdImage
#
# Then I dd this resulting image:
# $ sudo dd if=result/sd-image/<tab> of=/dev/mmcblk0 status=progress bs=16M
#
# In the root directory of this project
#
{ pkgs, ... }:
{
  imports = [
    # Import main configuration file
    ./configuration.nix
  ];

  # Don't compress image.
  sdImage.compressImage = false;

  # Override the firmware partiton building, this adds `core_freq=250`
  # to the `config.txt` file and runs a sed against `bootcode.bin` to
  # enable early boot uart.
  sdImage.populateFirmwareCommands =
    let
      extraConfigTxt = pkgs.writeText "extra-config.txt" ''
        # Downclock core freq to make the UART working. This is my only change from the
        # template in <nixpkgs/nixos/modules/installer/sd-card/sd-image-aarch64.nix> to
        # this file.
        core_freq=250
      '';
    in
    ''
      chmod +w firmware/config.txt firmware/bootcode.bin

      # Enable early boot uart working by patching bootcode.bin. This is my only change
      # from the template in <nixpkgs/nixos/modules/installer/sd-card/sd-image-aarch64.nix>
      # to this build script.
      sed -i -e "s/BOOT_UART=0/BOOT_UART=1/" firmware/bootcode.bin

      # Add extra data to config.txt
      cat ${extraConfigTxt} >> firmware/config.txt

      chmod -w firmware/config.txt firmware/bootcode.bin
    '';
}
