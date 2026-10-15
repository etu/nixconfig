{
  config,
  lib,
  pkgs,
  ...
}:
{
  options.etu.graphical.theme.enable = lib.mkEnableOption "Enable graphical theme settings";
  # macOS needs the real font family name and renders point sizes smaller than
  # Linux, so these defaults differ from the NixOS ones.
  options.etu.graphical.theme.fonts = {
    size = lib.mkOption {
      type = lib.types.int;
      default = 12;
      description = "Default font size";
    };
    monospace = lib.mkOption {
      type = lib.types.str;
      default = "JetBrains Mono";
      description = "Default monospace font to use";
    };
  };

  config = lib.mkIf config.etu.graphical.theme.enable {
    # Install the monospace font.
    fonts.packages = [
      pkgs.jetbrains-mono
    ];
  };
}
