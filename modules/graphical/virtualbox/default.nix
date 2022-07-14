{ config, lib, ... }:

{
  options.etu.graphical.virtualbox.enable = lib.mkEnableOption "Enable graphical virtualbox settings";

  config = lib.mkIf config.etu.graphical.virtualbox.enable {
    # Enable virtualbox.
    virtualisation.virtualbox.host.enable = true;

    # Add user to group
    etu.user.extraGroups = [ "vboxusers" ];

    # Enable persistence for virtualbox files.
    etu.base.zfs.user.directories = [
      ".config/VirtualBox"
      "VirtualBox VMs"
    ];
  };
}
