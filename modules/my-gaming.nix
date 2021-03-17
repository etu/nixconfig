{ config, lib, pkgs, ... }:
let
  cfg = config.my.gaming;

in
{
  options.my.gaming.enable = lib.mkEnableOption "Enables gaming related thingys.";

  config = lib.mkIf cfg.enable {
    # Enable 32bit libs for steam and such.
    hardware.opengl = {
      driSupport = true;
      driSupport32Bit = true;
    };

    nixpkgs.config.allowUnfree = true;

    # Support 32bit audio things
    services.pipewire.alsa.support32Bit = true;

    # Enable udev rules for steam controller
    services.udev.extraRules = ''
      SUBSYSTEM=="usb", ATTRS{idVendor}=="28de", MODE="0666"
      KERNEL=="uinput", MODE="0660", GROUP="users", OPTIONS+="static_node=uinput"
    '';

    # Steam link ports
    networking.firewall.allowedTCPPorts = [ 27036 27037 ];
    networking.firewall.allowedUDPPorts = [ 27031 27036 ];

    # List packages installed in system profile. To search by name, run:
    # $ nix-env -qaP | grep wget
    environment.systemPackages = with pkgs; [
      minecraft
      mumble
      sc-controller
      steam
    ];
  };
}
