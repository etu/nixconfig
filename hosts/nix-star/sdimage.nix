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
{
  config,
  pkgs,
  ...
}:
{
  imports = [
    # Import modules to build sd card images
    <nixpkgs/nixos/modules/installer/sd-card/sd-image.nix>

    # Import main configuration.nix
    ./configuration.nix
  ];

  # Populate firmware partition
  sdImage.populateFirmwareCommands =
    let
      configTxt = pkgs.writeText "config.txt" ''
        # For more options and information see
        # http://rpf.io/configtxtreadme
        # Some settings may impact device functionality. See link above
        # for details.

        # Specify kernel to boot.
        kernel=u-boot.bin

        # U-Boot needs this to work, regardless of whether UART is
        # actually used or not. Look in arch/arm/mach-bcm283x/Kconfig in
        # the U-Boot tree to see if this is still a requirement in
        # the future.
        enable_uart=1

        # Prevent the firmware from smashing the framebuffer setup done
        # by the mainline kernel when attempting to show low-voltage or
        # overtemperature warnings.
        avoid_warnings=1

        # Put in a boot delay before u-boot is loaded to avoid issues
        # where the radio chip interrupts u-boot.
        # boot_delay=5

        # Reserve less memory for the GPU (Only relevant on Pi0).
        gpu_mem=16

        # Attempt to get UART to work.
        dtoverlay=disable-bt
        dtoverlay=miniuart-bt

        # Uncomment some or all of these to enable the optional hardware interfaces
        #dtparam=i2c_arm=on
        #dtparam=spi=on

        # Disable Bluetooth to enable UART.
        #dtoverlay=miniuart-bt

        # D2RG UART over SPI
        #dtoverlay=sc16is752-spi0-ce0
      '';
    in
    ''
      # Make sure directory exists.
      mkdir -p firmware/overlays

      # Copy needed firmware files.
      for file in bcm2708-rpi-zero-w.dtb bcm2710-rpi-3-b.dtb bcm2710-rpi-3-b-plus.dtb bootcode.bin fixup_cd.dat start_cd.elf overlays/disable-bt.dtbo overlays/miniuart-bt.dtbo; do
        cp -v "${pkgs.raspberrypifw}/share/raspberrypi/boot/$file" "firmware/$file"
      done

      # Patch bootcode to listen to UART
      # sed -i -e "s/BOOT_UART=0/BOOT_UART=1/" firmware/bootcode.bin

      # Copy the config file.
      cp ${configTxt} firmware/config.txt

      # Copy the my raspberry pi 3 uboot bootloader.
      cp ${pkgs.ubootRaspberryPi3_64bit}/u-boot.bin firmware/u-boot.bin
      # Use this for pi 0: pkgs.ubootRaspberryPiZero
    '';

  sdImage.populateRootCommands = ''
    mkdir -p ./files/boot
    ${config.boot.loader.generic-extlinux-compatible.populateCmd} -c ${config.system.build.toplevel} -d ./files/boot
  '';

  # Don't compress the resulting image.
  sdImage.compressImage = false;
}
