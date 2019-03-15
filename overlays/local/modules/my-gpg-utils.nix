{ config, lib, pkgs, ... }:

with lib;

let
 cfg = config.my.gpg-utils;

in {
  options.my.gpg-utils.enable = mkEnableOption "Enables smartcard and gpg related utils that I use.";

  config = mkIf cfg.enable {
    services.pcscd.enable = true;

    environment.systemPackages = with pkgs; [
      # Install gnupg
      gnupg

      # Install pass with expensions
      (pass.withExtensions (ext: with ext; [ pass-otp pass-update ]))

      # Keysigning party
      signing-party
      msmtp
    ];
  };
}
