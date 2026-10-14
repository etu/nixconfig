{
  config,
  lib,
  perSystem,
  ...
}:
{
  options.etu.graphical.terminal = {
    enable = lib.mkEnableOption "Enable graphical terminal settings";
    terminalPackage = lib.mkOption {
      type = lib.types.package;
      default = perSystem.self.ghostty;
      description = "Terminal package to use.";
    };
    terminalPath = lib.mkOption {
      type = lib.types.str;
      default = "${config.etu.graphical.terminal.terminalPackage}/bin/${config.etu.graphical.terminal.terminalName}";
      description = "Path to terminal binary.";
      readOnly = true;
    };
    terminalName = lib.mkOption {
      type = lib.types.str;
      default = config.etu.graphical.terminal.terminalPackage.meta.mainProgram;
      description = "Binary name of the terminal.";
      readOnly = true;
    };
  };
}
