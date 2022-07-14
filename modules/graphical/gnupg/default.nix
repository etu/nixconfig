{ config, lib, pkgs, ... }:

{
  options.etu.graphical.gnupg.enable = lib.mkEnableOption "Enable graphical gnupg settings";

  config = lib.mkIf config.etu.graphical.gnupg.enable {
    # Enable smartcard deamon for yubikeys.
    services.pcscd.enable = true;

    # Enable gnupg agent.
    programs.gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
      pinentryFlavor = "gnome3";
    };

    home-manager.users.${config.etu.user.username} = lib.mkIf config.etu.user.enable {
      home.packages = [
        # Install gnupg
        pkgs.gnupg

        # Install pass with expensions
        (pkgs.pass.withExtensions (ext: with ext; [ pass-otp pass-update pass-checkup ]))

        # Keysigning party
        pkgs.signing-party
        pkgs.msmtp
      ];
    };

    # Enable persistence for gnupg and pass files.
    etu.base.zfs.user.directories = [
      ".gnupg"
      ".password-store"
    ];
    etu.base.zfs.user.files = [
      ".caffrc"
      ".msmtprc"
    ];
  };
}
