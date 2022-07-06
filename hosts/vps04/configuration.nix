# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, lib, pkgs, ... }:
let
  # Load secrets
  secrets = (import ../../data.nix).secrets;

  # Import my ssh public keys
  keys = (import ../../data.nix).pubkeys;

  # Import age secrets paths and metadata.
  ageModules = (import ../../data.nix).ageModules;

in
{
  imports = [
    # Include my hardware settings.
    ./hardware.nix

    # Include static network settings.
    ./networking.nix

    # Import local modules
    ../../modules
  ];

  # Set hostname
  networking.hostName = "vps04";

  # Settings needed for ZFS
  networking.hostId = "8425e390";

  # My module settings
  etu = {
    stateVersion = "20.09";

    base.emacs.enable = lib.mkForce false;
    development.git.enable = true;
    user.enable = true;
    user.extraAuthorizedKeys = keys.etu.weechat;
  };

  # Set up Sanoid for snapshots
  my.backup.enable = true;
  my.backup.enableSanoid = true;

  # Enable snapshotting for some filesystems
  services.sanoid.datasets."zroot/home".use_template = [ "home" ];
  services.sanoid.datasets."zroot/persistent".use_template = [ "persistent" ];

  # Disable documentation to make the system smaller.
  documentation.enable = false;
  documentation.doc.enable = false;
  documentation.info.enable = false;
  documentation.man.enable = false;

  # Install mosh
  programs.mosh.enable = true;

  # Install fish
  programs.fish.enable = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = [
    pkgs.git
    pkgs.htop
    pkgs.irssi
    pkgs.screen

    # Install weechat with weechat-matrix
    (pkgs.weechat.override {
      configure = { availablePlugins, ... }: {
        scripts = [ pkgs.weechatScripts.weechat-matrix ];
      };
    })

    # Install weechat-matrix helper scripts
    pkgs.weechatScripts.weechat-matrix
  ];

  # Enable aspell and hunspell with dictionaries.
  my.spell.enable = true;

  # Install tmux
  programs.tmux.enable = true;
  programs.tmux.clock24 = true;

  # List services that you want to enable:

  # Override identity paths for agenix since the openssh default paths
  # relies on a symlink being created in /etc/ssh to point at the
  # right path to make it to work as it would be in the right place.
  age.identityPaths = [
    "/persistent/etc/ssh/ssh_host_ed25519_key"
    "/persistent/etc/ssh/ssh_host_rsa_key"
  ];

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Enable my common cli utils
  my.common-cli.enable = true;

  # Allow password login for a user
  services.openssh.extraConfig = ''
    Match User ozeloten
      PasswordAuthentication yes
  '';
  security.pam.services.sshd.unixAuth = lib.mkForce true;
  services.openssh.kbdInteractiveAuthentication = false;

  # Enable a user to do deployments with
  my.deploy-user.enable = true;

  # Set up users accounts:
  users.users = {
    concate = {
      isNormalUser = true;
      home = "/home/concate";
      uid = 1001;
      openssh.authorizedKeys.keys = keys.concate;
    };

    talyz = {
      isNormalUser = true;
      shell = pkgs.fish;
      home = "/home/talyz";
      uid = 1002;
      openssh.authorizedKeys.keys = keys.talyz;
    };

    ozeloten = {
      isNormalUser = true;
      home = "/home/ozeloten";
      initialHashedPassword = secrets.hashedOzelotenPassword;
      uid = 1003;
    };

    bots = {
      home = "/home/bots";
      uid = 1004;
    };
  };

  # Enable the flummbot service
  services.flummbot.enable = true;
  services.flummbot.user = "bots";
  services.flummbot.group = "bots";
  services.flummbot.stateDirectory = "/home/bots";

  # Flummbot config file
  age.secrets.flummbot-toml = ageModules.flummbot-toml // {
    path = "${config.services.flummbot.stateDirectory}/flummbot.toml";
  };
}
