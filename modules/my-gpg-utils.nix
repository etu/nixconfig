{ config, lib, pkgs, ... }:
let
  cfg = config.my.gpg-utils;

in
{
  config = lib.mkIf cfg.enable {
    services.pcscd.enable = true;

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
  };
}
