{ pkgs, ... }:

{
  # Enable gpg related stuff
  my.gpg-utils.enable = true;

  # Enable common graphical stuff
  my.common-graphical.enable = true;
}
