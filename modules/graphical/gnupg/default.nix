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

    environment.systemPackages = with pkgs; [
      # Install gnupg
      gnupg

      # Install pass with expensions
      (pass.withExtensions (ext: with ext; [ pass-otp pass-update pass-checkup ]))

      # Keysigning party
      signing-party
      msmtp
    ];

    # Enable persistence for gnupg and pass files.
    environment.persistence."/persistent" = {
      users.${config.etu.user.username} = lib.mkIf config.etu.user.enable {
        files = [
          ".caffrc"
          ".msmtprc"
        ];
        directories = [
          ".gnupg"
          ".password-store"
        ];
      };
    };
  };
}
