{ pkgs, ... }:
{
  services.voxtype = {
    enable = true;
    package = pkgs.voxtype-vulkan;
    wayland.display = "wayland-1";

    settings.whisper.model = "small";
  };
}
