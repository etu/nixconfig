{ ... }:

{
  environment.etc."machine-id".source = "/persistent/etc/machine-id";

  environment.etc."ssh/ssh_host_rsa_key".source = "/persistent/etc/ssh/ssh_host_rsa_key";
  environment.etc."ssh/ssh_host_rsa_key.pub".source = "/persistent/etc/ssh/ssh_host_rsa_key.pub";
  environment.etc."ssh/ssh_host_ed25519_key".source = "/persistent/etc/ssh/ssh_host_ed25519_key";
  environment.etc."ssh/ssh_host_ed25519_key.pub".source = "/persistent/etc/ssh/ssh_host_ed25519_key.pub";

  # Avoid the need to have a moved config and help muscle memory of location
  environment.etc."nixos".source = "/persistent/etc/nixos";

  # Persistence of NetworkManager network connections.
  environment.etc."NetworkManager/system-connections".source = "/persistent/etc/NetworkManager/system-connections";

  # Persistence of roots dotfiles between boots
  fileSystems."/root" = {
    device = "/persistent/home/root";
    options = [ "bind" "noauto" "x-systemd.automount" ];
  };

  my.user.persistent = {
    extraFiles = [
      ".config/fish/fish_variables"

      # Work
      ".transifexrc"
      "bin/gh-md-toc"
      "bin/git-tvlog"
      "bin/mydump"
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

      # Work
      ".chef"
      ".config/Slack"
      ".config/VirtualBox"
      ".docker"
      ".kube"
      ".vagrant.d"
      "VirtualBox VMs"
      "tvnu"

      # Evolution
      ".config/evolution"
      ".config/goa-1.0"
      ".local/share/evolution"
      ".local/share/keyrings"
    ];
  };
}
