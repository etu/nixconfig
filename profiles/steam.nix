{ pkgs, ... }:

{
  # Enable 32bit libs for steam and such.
  hardware.opengl = {
    driSupport = true;
    driSupport32Bit = true;
  };

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    steam
  ];
}
