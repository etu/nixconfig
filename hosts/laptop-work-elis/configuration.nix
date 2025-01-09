# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{
  config,
  myData,
  ...
}: {
  imports = [
    # Include my hardware settings.
    ./hardware.nix
  ];

  # Set hostname
  networking.hostName = "laptop-work-elis";

  # Settings needed for ZFS
  networking.hostId = "18582528";

  # My module settings
  etu = {
    stateVersion = "24.05";

    base.fish.enableUserZoxideCd = true;
    development.enable = true;
    development.vscode.enableWork = true;
    graphical.enable = true;
    graphical.sway.enable = true;
    graphical.virtualbox.enable = true;
    graphical.flatpak.enablePersistence = true;
    services.nfs.enable = true;
    services.nfs.exports = ''
      ${config.etu.dataPrefix}/home/etu/tvnu/projects 192.168.5.102(rw,no_subtree_check,all_squash,anonuid=1000,anongid=100)
    '';
    services.netdata.enable = true;
    theme.enable = true;
    user.enable = true;
    user.extraGroups = ["video" "docker"];

    # Allow home fileserver to connect to fetch snapshots.
    user.extraRootAuthorizedKeys = myData.pubkeys.etu.syncoid.server-main-elis;

    # Install extra modes for work.
    base.emacs.extraConfig = [
      ''
        ;; Install Elasticsearch mode
        (use-package es-mode :ensure t)

        ;; Install Jenkinsfile mode
        (use-package jenkinsfile-mode :ensure t)

        ;; Install VCL mode
        (use-package vcl-mode :ensure t)
      ''
      # Install copilot.el
      ''
        ;; Copilot.el
        (use-package copilot
          :ensure t
          :hook ((dockerfile-mode php-mode nix-mode go-mode lisp-mode yaml-mode markdown-mode) . copilot-mode)
          :bind (("C-c M-f" . copilot-complete)
                 :map copilot-completion-map
                 ("C-g" . 'copilot-clear-overlay)
                 ("M-p" . 'copilot-previous-completion)
                 ("M-n" . 'copilot-next-completion)
                 ("<tab>" . 'copilot-accept-completion)
                 ("M-f" . 'copilot-accept-completion-by-word)
                 ("M-<return>" . 'copilot-accept-completion-by-line)))
      ''
    ];

    base.sanoid.datasets = {
      # Enable snapshotting for some filesystems
      "zroot/safe/data".use_template = ["data"];
      "zroot/safe/home".use_template = ["home"];
    };

    # Enable work modules
    work.enable = true;
  };

  # Enable docker deamon
  virtualisation.docker.enable = true;
  virtualisation.docker.storageDriver = "zfs";

  # Include agenix encripted secret for secret password file
  age.secrets = {
    inherit (myData.ageModules) syncoid-workstations-ssh-ec;
  };

  # Enable ClamAV.
  services.clamav.daemon.enable = true;
  services.clamav.updater.enable = true;

  # Enable blueman.
  services.blueman.enable = true;
}
