{
  config,
  lib,
  pkgs,
  ...
}: {
  options.etu.services.jellyfin = {
    enable = lib.mkEnableOption "Enable services jellyfin service";
    hostname = lib.mkOption {
      type = lib.types.str;
      default = "jellyfin.elis.nu";
      description = "Hostname to expose jellyfin on";
    };
  };

  config = lib.mkIf config.etu.services.jellyfin.enable {
    # Bind mount for persistent data for jellyfin
    etu.base.zfs.system.directories = [
      "/var/lib/jellyfin"
    ];

    # Enable vaapi on OS-level
    nixpkgs.config.packageOverrides = pkgs: {
      vaapiIntel = pkgs.vaapiIntel.override {enableHybridCodec = true;};
    };
    hardware.graphics = {
      enable = true;
      extraPackages = [
        pkgs.intel-media-driver
        pkgs.vaapiIntel
        pkgs.vaapiVdpau
        pkgs.libvdpau-va-gl
      ];
    };

    # Allow jellyfin user to access the graphics card
    users.users.${config.services.jellyfin.user}.extraGroups = ["video" "render"];

    # Override default hardening measure from NixOS
    systemd.services.jellyfin.serviceConfig.PrivateDevices = lib.mkForce false;
    systemd.services.jellyfin.serviceConfig.DeviceAllow = lib.mkForce ["/dev/dri/renderD128"];

    # Enable jellyfin itself
    services.jellyfin.enable = true;

    # Open NGiNX port
    networking.firewall.allowedTCPPorts = [80 443];
  };
}
