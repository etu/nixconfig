{ lib, ... }:
{
  # Font settings, read by homeModules.emacs (and mirrored by the darwin
  # module of the same name).
  options.etu.graphical.theme.fonts = {
    size = lib.mkOption {
      type = lib.types.int;
      default = 10;
      description = "Default font size";
    };
    biggerSize = lib.mkOption {
      type = lib.types.int;
      default = 13;
      description = "Bigger font size";
    };
    monospace = lib.mkOption {
      type = lib.types.str;
      default = "JetBrainsMono";
      description = "Default monospace font to use";
    };
    normal = lib.mkOption {
      type = lib.types.str;
      default = "DejaVu Sans";
      description = "Default font to use";
    };
  };
}
