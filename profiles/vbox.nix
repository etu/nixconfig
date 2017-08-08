{ pkgs, ... }:

{
  # Enable virtualbox.
  virtualisation.virtualbox.host.enable = true;

  # Add user to group
  users.extraUsers.etu.extraGroups = [ "vboxusers" ];
}
