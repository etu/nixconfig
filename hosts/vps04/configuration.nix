# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ./networking.nix
    # Import local modules & overlays
    ../../overlays/local/default.nix
  ];

  # Use the GRUB 2 boot loader.
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.device = "/dev/sda";

  networking.hostName = "vps04";

  nix.nixPath = [
    "nixpkgs=/nix/var/nix/profiles/per-user/root/channels/nixos"
    "nixos-config=/nix/persistent/etc/nixos/configuration.nix"
  ];

  # Select internationalisation properties.
  # i18n = {
  #   consoleFont = "Lat2-Terminus16";
  #   consoleKeyMap = "us";
  #   defaultLocale = "en_US.UTF-8";
  # };

  # Auto upgrade system
  system.autoUpgrade.enable = true;
  system.autoUpgrade.channel = "https://nixos.org/channels/nixos-19.03-small";
  system.autoUpgrade.dates = "weekly";

  # Auto garbage collect
  nix.gc.automatic = true;
  nix.gc.options = "--delete-older-than 30d";

  # Auto update the config before it upgrades the system
  my.update-config.enable = true;
  my.update-config.path = "/nix/persistent/etc/nixos/";

  # Set your time zone.
  time.timeZone = "Europe/Stockholm";

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    git htop screen weechat irssi
  ];

  # Enable aspell with dictionaries.
  my.aspell.enable = true;

  # Install tmux
  programs.tmux.enable = true;
  programs.tmux.clock24 = true;

  # Install mosh
  programs.mosh.enable = true;

  # Install fish
  programs.fish.enable = true;

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;
  services.openssh.hostKeys = [
    { type = "rsa";     path = "/nix/persistent/etc/ssh/ssh_host_rsa_key"; bits = 4096; }
    { type = "ed25519"; path = "/nix/persistent/etc/ssh/ssh_host_ed25519_key"; }
  ];

  # Set up users accounts:

  users.mutableUsers = false;

  users.users = {
    root.initialHashedPassword = "$6$jpUfLDIhIOQ5qq4b$yj55LOEIUZZKQMzO4UKqSnEJO0n8m5loDM/X7L2a5R0G6p0DxLLh.Y5pQ2Q1qEv8vwd0fR6bPHGRA76JbHXwK.";

    etu = {
      isNormalUser = true;
      extraGroups = [ "wheel" ];
      shell = pkgs.fish;
      home = "/nix/persistent/home/etu";
      initialHashedPassword = "$6$jpUfLDIhIOQ5qq4b$yj55LOEIUZZKQMzO4UKqSnEJO0n8m5loDM/X7L2a5R0G6p0DxLLh.Y5pQ2Q1qEv8vwd0fR6bPHGRA76JbHXwK.";
      uid = 1000;
      openssh.authorizedKeys.keys = [
        "no-agent-forwarding,no-X11-forwarding,permitopen=\"localhost:8001\",command=\"echo 'This account can only be used for weechat relays'\" ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDELIFRmA2C93sjHebScoj8QPynqdYyl6fgYLOrBMsBKQAKrzsfF4wmA/LYo9Z89l3TmpMqzd4C/315HFO6sO7iHVrUfsC0lToA+FOcN7D40pr8m+AaQtVSI14Mlz4GY3fyeyYyssz7XXMn9LEzgZ8SxZh06YLJM9yL1kprBoRXe3Bxbja38JBSl+8xBWRyNrQBPySrTeuoxRYbJ8DUwtOeSElSP6YDjtMut4PbjLXJ2GNHavXhoQaLiZsW4c4YzcMzjiKEmAZWNg2cNuljXMf3KoKCbxqiD9zWidWhKdMuT+XhuDzTt89JAdWWStkj2N++eeESRozHmDBp9PROJx7Z etu@android-2017-04-24"
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILPvVYtcFHwuW/QW5Sqyuno7KrsVq9q9HUOBoaoIlIwu etu@hactar-2016-09-24"
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPXaF1OwJAyGuPr3Rb0E+ut1gxVenll82/fLSc7p8UeA etu@fenchurch-2017-07-14"
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINvIdD5t0Tjn+e41dIMt9VM5B0gs9yCuTY4p7Hpklrhr etu@ford-2018-03-05"
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQC30zMfq5/ZBnLPXiz4qtTsg3SU6voKQMumADNhTpVSKo3erANR5zTb1WPfjM4IWLCcWfksWDNOOeMaKM0hGgdnGbfrXpIOJwKNHaSp11cvQ6wTMAGV3B3ItJHOV+Czw4kEeUB+Tic8m+U2jnTPLXC4x3B7bdXHhdhmQbTpEq9pabe8eRQM54/9SuG6M9y8G3g35s3edsXrEnh/OI62a66F5aOugQH4fX5ehfGg3zk7LLu7U8bX9FGOeOVCCEBsfm5ysczNAO3v1iA4G9N8vgfAHJNZfKglYSQIi9nyURxcqT511OFTGK1cyWHGjqCNK286Plx90u0SVQvvG+9hkq2l/kbgmNpEYlTmAs22y+6j7R+gpSCMxJSfjXfJeyVcCTKo2CT9+SQRDz+pz+wyv/NgnaqXuP65RlwS0OIhdT6YheaFfbhkuMzFD78VDWOacLamWVQz/yTe5o+GhTavWVZyZ4Y9Wf+LB4sQtM9S5AqWpSCHDfA9nF6E9oWPbAZ4l+VD4vNQdrmByh+3uk+XP9/ytJVyJDd88MmxnC1yiN3xT7rQaUoUCmYy0Z7BLBYvBb2fU+JrJ8Eew6uf23SGYUtZuxanNANslBdvD0t68xCEHIxsVecpqRtbG8699bZEoWWbAFS1WO5EBAcYRfKV/4SQxLTO0J+HGQSRkgL7Ex1ioQ== etu@phouchg-4096-2016-04-18"
      ];
    };

    concate = {
      isNormalUser = true;
      home = "/nix/persistent/home/concate";
      uid = 1001;
      openssh.authorizedKeys.keys = [
        "no-agent-forwarding,no-X11-forwarding,permitopen=\"localhost:8002\",command=\"echo 'This account can only be used for weechat relays'\" ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDQqvuRrzAcmp2neNQi/gDEQy6pe9EF2+pDWOTjmdC3+JDSOnZMHm9PX5e7YLZNk46YMSvia0XALQ16/Ao1xqsgyC1YRa8lWOxFn/8rmMIStSO2HKzX6LNJafqlFm5FAJO7znYYXImB0JHOOF43P8RzxQZ+X/MCt0SKYxsM8JeBKFzyDqlzxA7pqTH81DMENL9xT1rlaElg86JmTJWcQWriJVCCf6wi5bSlArCghQ1gU8oOhDGGB9OhCMj9KeumrbVzyZO5e03anX066bQnfTve8Tg1pxCXPse1kzyqkHrD4mVTA2jmVkiPT72WkZOiegOEi3CA1DfKUIfiFw5i6KNj conacte@android-2048-2018-02-21"
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIE/CqVKQd8Bk/e99BQ6floXFMhNZ3d3anPIKl9+RyXzO concate@gaara.2018-07-13"
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDSpRskzDJwIYz6uVK9Yvl0B2I8zlnVC4KOOu/Jr+bhTW9aXvbrhVd6y1u+vicJ78j25MvzH3CXQY0D59CEnYYD1QlHIMN3fRICcrcbTNIhGaeVsTBrzvgeeMLluzTCe0fyve7lY2P1drn55lA+Mg6Au8oSho2904nOjiCcB/glnQ38Mu4nvyjm/dVBw83Dq2sOl+gyCtsTlIv6Gc7r7AafGComXDVrVbicPd2ig654rtz7L1LoCye298CJLXUJhJolfxhP+F4GDAqdjkj4o3MwlYrv6HNu6LYzcWKlagkL5m5LhSYC0uPytXWHYRO/mIvwQmFOlAI1sQ/q/ynHtFnp concate@hei"
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIH4+d3/gP+eiOTap7+u5tfX94ijF65kA0ilMryZYBuHD concate@hactar.2016-10-15"
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIB7kQjFkos4fpNsGgHY861B5mddRo2EDOYe7HkHPRZ2f concate@rocklee-2018-12-31"
      ];
    };

    talyz = {
      isNormalUser = true;
      shell = pkgs.fish;
      home = "/nix/persistent/home/talyz";
      uid = 1002;
      openssh.authorizedKeys.keys = [
        "no-agent-forwarding,no-X11-forwarding,permitopen=\"localhost:8003\",command=\"echo 'This account can only be used for weechat relays'\" ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDZKYLFI5eyv4/aZR8YUBLQagMoH4gyltcXiom/ZA9XpgtKy3fOxmX0NzETxVBdbc/mhU6s9zKNBXWDl5OS17a+GNPStPpUjMGB/ehTK+FkcNOHsqZ60QLHzejWbMfOFiZ9idPsVpvWl8o95YzUAZc61NlVMR1r9fys1CZtFk6ZfYuGLofnG3BafCeMY6BkWZ7Hi/+AoWxe57CIJzwmy3aBpQ7NxpS5vZl8/DGThLuRK4Ew6w1/TmRTTJjTr0USKPSI1V3XYZDkN3BZ4dRyIerpMEPhYfsLum1Qj+Oc60EziERsSadk4UnJt3ye4VjTcv+1za/CeDz6zAjgpEbfEBAN8xpNsfdjeFedw32YGRFhWpC2xg/yho7c2n8w5IOWKObggKZYxLBatzKpg0eYO5B/jBNnM6HIW/wfiKunnYrJMJHwzFSYZCyIRlZKb6yWRcW1+RlvlmlE95bAxPWmrFbEtOoPVESAR6lfTleKPMgjTGJ3KvYzp1k8jplJcQpbPmMiejkheh3tcKWn/Na3EfV3pUWGHtU6CQauHYdDBAfHnAD9baAHEpy3HGYz+0cgH4kKSas4U8ICaFeOE3OEv90caHyGHWaL5ynvq4ywUAlJUWYLIseHq4+aSxKj8aK/Nr6zfDUNAAOEaveT/YUeFk5LxaEXowkW/98nUwZ//G7vKw== talyz@android-2017-03-03"
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPqiNApj9AcGJ4JfsDTdhSlfwmuDzKJmVrOGJibYKOo5 root@raine-2017-02-18"
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIC0Ny2gtIq74qiHph/5ZUyNkdVTTXj2lnuDRHpgR3fPi talyz@flora-2017-01-10"
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQC5RDs62krOSjnSS26y9nPpP0jpIHSO9uQ1tMVd4kAuQlAp0Q1Nfq9YwImXccdDl/wEycreT17wRLO9Oq8w2Nlnp4d7Xe+5acadcCSNx0rd8773357/n/QK2GyPa9ysDP3HnNLK6QhBf4GVdtCoHflk1liONzSdhiCGPHvpYFowG67cZTzb4/YPvryS//478OEa2JutTrP+N7fjH+SHuzndiWSNvWXTlqXWIufXKPqFzaKueLuABUSLe/Sj7ZNJhcMrBPUKmtKCQSbrAmdYvk5WzX6FacMlUWACNDceXH31u7OVLXDjQYU2jlc5E3s60erNlHlLtRGSd+wqCQv21ttlhCi9EVqdByEGv9ZGjt9lq4GS0ZHZCHTts17hjYSYql2QnYyx5ypSLOyqSjA5n34V3QUcVCCBk8QCt4wrWKqmU79TSOK0ihCaZiYlicQcKUgGdU/ouGgWp9+fUu4BqNKG8EIEmgIgbvJoe0QohDTJ9EyAkxMUz3L6KhFpVv3Zd470pChYgCMsjowrKJAWzq75UszCelRTFIylmE7LmAo9OSU8MHSD3ihf07NNXa0rw1t15pMOFjCwQWj2ea3tBeUUVLb4BTdiH4P//5lsHckeA6d1xtniP7HQXJ3Vft6IL+igZ/npL4yK412yMXm05ufzfG8dsuMW4k9eVQbXBQeeOQ== talyz@natani-2018-02-12"
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQClEBWe28hb+GRtpa0MdKh/2UKPIANBEeDuXlcfHgS4Mi4gTNKMWjclXhBzEYfRLpYEZuBjcciRTKNKUjMi4aSmc3g6/FoueaDTmHhoQWCEFwrR7m1ZwplHYZuad2Dm6kOMXyi3VIw1y4u3K832LsTrrgBeDT+C23qVfjvmeytD7tWnoEqgDKnrdxZYqiNdu43HA5V7r7jXCMVby2/39iqa+AxKBxt/v1gz7rar3jr/6EfE55oJpQfj8wFGLq88IK915eTTEVYSZLUxZkfOaZGMjkyMiXNTLWtJ/MfBQ0SagDwwuZKf/+C/O1vO6scz6Uc9wBUPPbUnhUkGzO/kWduXiXLQfEwYrFVAd2HyrErwnuJs/0HSWYm/c6o4O1xaDaqj+bcfGewK+EEPU+J1P0MwXTLgpJ6r0VkzrKd/r0kVUrxXqhrMdSwtl6M3CgqDc3rFgiV5xs4nRjnwbhchud77ktZ3zV40uLYXHa5IlldN4O91MD1+LVffc5eceJmhn9ivuoEk+w/Wwtk8c/G2axakfmF9H4VFRgzyVnKrel2Gz4gZd1wihA2B8o4eh10pEmeS5O0BRDXpJGMC3FKCelX42mEYy4qr6bCF4Bqo0+bQOHgzZpdQQ+utmvrYlMVVcJMqh2xjSbaPdC+trOa0fvVBFTXIAF/Wn/1zFj6+G6mCYQ== talyz@zen-2018-11-16"
      ];
    };

    ozeloten = {
      isNormalUser = true;
      home = "/nix/persistent/home/ozeloten";
      initialHashedPassword = "$6$Tp21Uo367npe/$/a6taUbyYu3QQo8RAPp6krKAq8wNs67hqSc0KPCzT32N7Aqkud110qddUAywOGKdYPJc/23BqogmUpVBQEoGF/";
      uid = 1003;
    };

    bots = {
      isNormalUser = true;
      home = "/nix/persistent/home/bots";
      uid = 1004;
    };
  };

  # Enable the flummbot service
  programs.flummbot.enable = true;
  programs.flummbot.stateDirectory = "/nix/persistent/home/bots";

  # A hack to `loginctl enable-linger m` (for multiplexer sessions to last),
  # until this one is resolved: https://github.com/NixOS/nixpkgs/issues/3702
  system.activationScripts.loginctl-enable-linger-m = pkgs.lib.stringAfter [ "users" ] ''
    ${pkgs.systemd}/bin/loginctl enable-linger etu
    ${pkgs.systemd}/bin/loginctl enable-linger concate
    ${pkgs.systemd}/bin/loginctl enable-linger talyz
  '';

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "18.09"; # Did you read the comment?
}
