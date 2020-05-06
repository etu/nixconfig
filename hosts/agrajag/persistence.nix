{ ... }:

{
  environment.etc."machine-id".source = "/persistent/etc/machine-id";

  environment.etc."ssh/ssh_host_rsa_key".source = "/persistent/etc/ssh/ssh_host_rsa_key";
  environment.etc."ssh/ssh_host_rsa_key.pub".source = "/persistent/etc/ssh/ssh_host_rsa_key.pub";
  environment.etc."ssh/ssh_host_ed25519_key".source = "/persistent/etc/ssh/ssh_host_ed25519_key";
  environment.etc."ssh/ssh_host_ed25519_key.pub".source = "/persistent/etc/ssh/ssh_host_ed25519_key.pub";

  # Avoid the need to have a moved config and help muscle memory of location
  fileSystems."/etc/nixos" = {
    device = "/persistent/etc/nixos";
    options = [ "bind" "noauto" "x-systemd.automount" ];
  };

  # Persistence of NetworkManager network connections.
  fileSystems."/etc/NetworkManager/system-connections/" = {
    device = "/persistent/etc/NetworkManager/system-connections/";
    options = [ "bind" "noauto" "x-systemd.automount" ];
  };

  my.user.persistent = {
    extraFiles = [
      ".config/fish/fish_variables"
    ];
    extraDirectories = [
      ".caffrc"
      ".dotfiles"
      ".gnupg"
      ".local/share/TelegramDesktop/tdata"
      ".local/share/dino"
      ".local/share/direnv"
      ".local/share/emacs"
      ".local/share/fish"
      ".mozilla/firefox"
      ".msmtprc"
      ".password-store"
      ".ssh"
      "Downloads"
      "code"
      "documents"
      "org"

      # Evolution
      ".config/evolution"
      ".config/goa-1.0"
      ".local/share/evolution"
      ".local/share/keyrings"
    ];
  };
}
