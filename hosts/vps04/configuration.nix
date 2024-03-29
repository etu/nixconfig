# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{
  config,
  lib,
  myData,
  pkgs,
  ...
}: {
  imports = [
    # Include my hardware settings.
    ./hardware.nix

    # Include static network settings.
    ./networking.nix
  ];

  # Set hostname
  networking.hostName = "vps04";

  # Settings needed for ZFS
  networking.hostId = "8425e390";

  # Enable weekly garbage-collection and daily store optimization.
  nix.gc.automatic = true;
  nix.gc.dates = "weekly";
  nix.gc.options = "--delete-older-than 7d";
  nix.optimise.automatic = true;
  nix.optimise.dates = ["daily"];

  # My module settings
  etu = {
    stateVersion = "24.05";

    base.emacs.enable = false; # Disable emacs that is enabled by default.

    user.extraRootAuthorizedKeys =
      # Allow home server to pull backups
      myData.pubkeys.etu.syncoid.server-main-elis
      ++
      # Allow github to deploy system
      myData.pubkeys.etu.github-actions;

    base.sanoid.datasets = {
      # Enable snapshotting for some filesystems
      "zroot/safe/data".use_template = ["data"];
      "zroot/safe/home".use_template = ["home"];
    };
    services.netdata.enable = true;
  };

  users.motd = ''
    ===================================================================
    Notis: Denna server planeras att stängas ner utan någon planerad
           ersättare. Detta är på grund av att Elis har slutat
           använda IRC direkt sedan en längre tid tillbaka och tycker
           inte att det är så kul att sitta och betala för denna
           server varje månad.

           Det finns ingen hård dödslinje på när den kommer att stängas
           ner ännu. Men ni som fortfarande är beroende av
           funktionaliteten av denna server kan väl kanske samordna en
           ersättare till er.
    ===================================================================
  '';

  # Disable documentation to make the system smaller.
  documentation.enable = false;
  documentation.doc.enable = false;
  documentation.info.enable = false;
  documentation.man.enable = false;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = [
    pkgs.git
    pkgs.htop
    pkgs.irssi
    pkgs.screen

    # Install weechat with weechat-matrix
    (pkgs.weechat.override {
      configure = _: {
        scripts = [pkgs.weechatScripts.weechat-matrix];
      };
    })

    # Install weechat-matrix helper scripts
    pkgs.weechatScripts.weechat-matrix
  ];

  # List services that you want to enable:

  # Allow password login for a user
  services.openssh.extraConfig = ''
    Match User ozeloten
      PasswordAuthentication yes
  '';
  security.pam.services.sshd.unixAuth = lib.mkForce true;
  services.openssh.settings.KbdInteractiveAuthentication = false;

  # Set up users accounts:
  users.users = {
    talyz = {
      isNormalUser = true;
      shell = pkgs.fish;
      home = "/home/talyz";
      uid = 1002;
      openssh.authorizedKeys.keys = myData.pubkeys.talyz;
    };

    ozeloten = {
      isNormalUser = true;
      home = "/home/ozeloten";
      hashedPasswordFile = config.age.secrets.hashed-ozeloten-password.path;
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
  age.secrets.flummbot-toml =
    myData.ageModules.flummbot-toml
    // {
      path = "${config.services.flummbot.stateDirectory}/flummbot.toml";
    };
  age.secrets.hashed-ozeloten-password = myData.ageModules.hashed-ozeloten-password;
}
