{
  config,
  lib,
  osConfig,
  pkgs,
  ...
}:
{
  # Enable emacs in home-manager
  home.file.".emacs".text = "(setq-default inhibit-startup-screen t)";
}
