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

        # Signing party for the tool caff
        pkgs.signing-party
      ];

      home.file = {
        ".caffrc".text = ''
          # .caffrc -- vim:ft=perl:
          # This file is in perl(1) format - see caff(1) for details.

          $CONFIG{'owner'} = '${config.etu.user.realname}';
          $CONFIG{'email'} = '${config.etu.user.email}';

          # I used to have the last 16 chars of my key's fingerprint
          # here but have replaced it with the full fingerprint. This
          # is a note to the future if this is a problem.
          $CONFIG{'keyid'} = [ qw{${config.etu.user.signingKey}} ];

          $ENV{'PERL_MAILERS'} = 'sendmail:${pkgs.msmtp}/bin/msmtp';
        '';
        ".msmtprc".text = ''
          account default
          host smtp.gmail.com
          tls on
          tls_certcheck on
          tls_trust_file /etc/ssl/certs/ca-certificates.crt
          auth on
          user ${config.etu.user.email}
          from ${config.etu.user.email}
          passwordeval "pass authinfo | grep -e 'smtp ${config.etu.user.email}' | cut -d ' ' -f 3"
          port 587
        '';
      };
    };

    # Enable persistence for gnupg and pass files.
    etu.base.zfs.user.directories = [
      ".gnupg"
      ".password-store"
    ];
  };
}
