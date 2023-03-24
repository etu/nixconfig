# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{myData, ...}: {
  imports = [
    # Include my hardware settings.
    ./hardware.nix
  ];

  # Set hostname
  networking.hostName = "zparv-zerver";

  # Settings needed for ZFS
  networking.hostId = "48a0ce30";

  # Enable weekly garbage-collection and daily store optimization.
  nix.gc.automatic = true;
  nix.gc.dates = "weekly";
  nix.gc.options = "--delete-older-than 7d";
  nix.optimise.automatic = true;
  nix.optimise.dates = ["daily"];

  # My module settings
  etu = {
    stateVersion = "23.05";

    # Set data prefix so agenix can find the host keys.
    dataPrefix = "/";

    # We do have a persistent root on this system, I know, it's
    # weird. So here I disable the persstence settings for this system
    # and just keep the files on /
    base.zfs.enable = false; # Disable emacs that is enabled by default.

    base.nix.allowUnfree = ["minecraft-server"];

    base.emacs.enable = false; # Disable emacs that is enabled by default.
    base.sanoid.datasets = {
      # Enable snapshotting for some filesystems
      "zroot/safe/root".use_template = ["data"];
      "zroot/safe/home".use_template = ["data"];
      "zroot/local/minecraft".use_template = ["data"]; # Minecraft server
    };

    # Allow github to deploy system
    user.extraRootAuthorizedKeys = myData.pubkeys.etu.github-actions;

    services.netdata.enable = true;
  };

  # Disable documentation to make the system smaller.
  documentation.enable = false;
  documentation.doc.enable = false;
  documentation.info.enable = false;
  documentation.man.enable = false;

  # Allow inbound traffic to lancache ports.
  networking.firewall.allowedTCPPorts = [80 443];
  networking.firewall.allowedUDPPorts = [53];

  # Set up docker.
  virtualisation.docker.enable = true;
  virtualisation.docker.storageDriver = "zfs";

  # Set up lancache docker container.
  virtualisation.oci-containers.backend = "docker";
  virtualisation.oci-containers.containers = {
    lancache = {
      image = "lancachenet/monolithic:latest";
      ports = [
        "80:80/tcp"
        "443:443/tcp"
      ];
      volumes = [
        "/media/zstorage/lancache/data:/data/cache"
        "/media/zstorage/lancache/logs:/data/logs"
      ];
    };
    lancache-dns = {
      image = "lancachenet/lancache-dns:latest";
      ports = [
        "53:53/udp"
      ];
      environment = {
        LANCACHE_IP = "10.69.0.3";
        USE_GENERIC_CACHE = "true";
        UPSTREAM_DNS = "1.1.1.2";
      };
    };
  };

  # Set up a minecraft server
  services.minecraft-server = {
    enable = true;
    eula = true;
    openFirewall = true;
    declarative = true;

    serverProperties = {
      server-port = 25565;
      motd = "Välkommen till Sparv's Minecraft server!";
      difficulty = "normal";
      gamemode = "survival";
      max-players = 10;
      snooper-enabled = false;

      enable-command-block = true;
      online-mode = true;
      white-list = true;

      # Disable spawn protection.
      spawn-protection = 0;

      # Enable password to allow to connect.
      # enable-rcon = true;
      # "rcon.password" = "hunter2";
    };

    # Enable to allow users to connect.
    # Example:
    # - "username" = "uuid"
    whitelist = {
      "etuetuetu" = "e5520c26-81b6-4683-9ed0-53dc8b5f4d3f";
      "Angrontyr" = "5a88e4f6-56c1-4f16-93d7-34bd6aea3d74";
      "PikabooSuprise" = "3357057f-77ee-45b5-a0c5-5f5b303b0e02";
      "Eiydra" = "47fa8f5f-f7d3-441a-8c12-4306372ee81b";
      "LightCatcher0_0" = "b1a5906a-2f2e-4a51-b273-2b0e164eafde";
      "Steelwolf16" = "e0000732-519e-46ac-9d1c-b4b2f460534f";
      "concate" = "776ddc3d-f182-4b8c-a07e-ae4b355994d8";
      "ScarletHunter22" = "1cf53c7c-75c3-480a-b91a-8a64fe84a3cc";
      "Pralin" = "72343c63-2442-4333-8e97-72c98d691905";
    };
  };

  # Hacks from the last LAN.
  systemd.services = let
    buildVlcChromecast = streamUrl: chromecastIp: port: {
      description = "Service to start a streaming ${streamUrl} to chromecast ${chromecastIp}";
      after = [ "network-online.target" ];
      environment = { DISPLAY = ":0"; };
      preStart = ''
        echo "Checking for nameservers to appear in /etc/resolv.conf";
        while ! grep nameserver /etc/resolv.conf > /dev/null; do
          echo "Waiting for nameservers to appear in /etc/resolv.conf";
          sleep 1;
        done
      '';
      serviceConfig = {
        User = "etu";
        ExecStart = pkgs.writeScript "play.sh" ''
          #!${pkgs.bash}/bin/sh
          ${pkgs.vlc}/bin/cvlc ${streamUrl} --sout='#chromecast{ip=${chromecastIp}}' --demux-filter=demux_chromecast --sout-chromecast-http-port ${port}
        '';
        TimeoutStopSec = "1";
      };
    };

    announce = "https://www.youtube.com/watch?v=dQw4w9WgXcQ"; # Currently rickroll
    cameraLower = "rtsp://sparv.failar.nu:7447/pbin9NlvEBnzmUGv";
    cameraUpper = "rtsp://sparv.failar.nu:7447/xiCcyx7QhrlA9xFQ";
    rickroll = "https://www.youtube.com/watch?v=dQw4w9WgXcQ";
    twitchSparv = "$(${pkgs.yt-dlp}/bin/yt-dlp https://www.twitch.tv/speliarvika --get-url)";
    twitchSpiinken = "$(${pkgs.yt-dlp}/bin/yt-dlp https://www.twitch.tv/spiinken --get-url)";
  in {
    # Chromecast the cameras
    vlc-chromecast-upper = (buildVlcChromecast cameraLower "10.69.0.31" "8010") // { wantedBy = [ "multi-user.target" ]; };
    vlc-chromecast-lower = (buildVlcChromecast cameraUpper "10.69.0.110" "8011") // { wantedBy = [ "multi-user.target" ]; };

    # Rickroll
    vlc-chromecast-rickroll-upper = buildVlcChromecast rickroll "10.69.0.31" "8012";
    vlc-chromecast-rickroll-lower = buildVlcChromecast rickroll "10.69.0.110" "8013";

    # Announcement
    vlc-chromecast-announce-upper = buildVlcChromecast announce "10.69.0.31" "8014";
    vlc-chromecast-announce-lower = buildVlcChromecast announce "10.69.0.110" "8015";

    # Twitch Spiinken
    vlc-chromecast-twitch-spiinken-upper = buildVlcChromecast twitchSpiinken "10.69.0.31" "8016";
    vlc-chromecast-twitch-spiinken-lower = buildVlcChromecast twitchSpiinken "10.69.0.110" "8017";

    # Twitch Spiinken
    vlc-chromecast-twitch-sparv-upper = buildVlcChromecast twitchSparv "10.69.0.31" "8018";
    vlc-chromecast-twitch-sparv-lower = buildVlcChromecast twitchSparv "10.69.0.110" "8019";
  };

  # Set up admin interface
  services.phpfpm.pools."foobar" = {
    user = "nginx";
    phpPackage = pkgs.php;
    settings = {
      "listen.group" = "nginx";
      "listen.mode" = "0600";
      "listen.owner" = "nginx";
      "pm" = "dynamic";
      "pm.max_children" = 5;
      "pm.max_requests" = 500;
      "pm.max_spare_servers" = 3;
      "pm.min_spare_servers" = 1;
      "pm.start_servers" = 2;
    };
  };

  security.sudo.extraConfig = "nginx ALL=(ALL) NOPASSWD:ALL";

  # Using nginx for rtmp pushing / proxying may be interesting in the future.
  # https://github.com/nixcon/nixcon-video-infra/blob/e47f0ecc0a685bad42532241d7d045a75bb20c6f/modules/rtmp-push.nix
  services.nginx = {
    enable = true;

    virtualHosts."phpfpm" = let
      testdir = pkgs.writeTextDir "web/index.php" ''
        <?php
        $validServicePrefixes = [
          'vlc-chromecast' => 'Cameras',
          'vlc-chromecast-announce' => 'Announces',
          'vlc-chromecast-rickroll' => 'Rickrolls',
          'vlc-chromecast-twitch-sparv' => 'Twitch Sparv',
          'vlc-chromecast-twitch-spiinken' => 'Twitch Spiinken',
        ];
        foreach (array_keys($validServicePrefixes) as $validServicePrefix) {
          if (isset($_GET['play']) && $_GET['play'] === $validServicePrefix) {
            system("/run/wrappers/bin/sudo ${pkgs.systemd}/bin/systemctl restart ".$validServicePrefix."-upper");
            system("/run/wrappers/bin/sudo ${pkgs.systemd}/bin/systemctl restart ".$validServicePrefix."-lower");
            foreach (array_keys($validServicePrefixes) as $validPrefix) {
              if ($_GET['play'] !== $validPrefix) {
                system("/run/wrappers/bin/sudo ${pkgs.systemd}/bin/systemctl stop ".$validPrefix."-upper");
                system("/run/wrappers/bin/sudo ${pkgs.systemd}/bin/systemctl stop ".$validPrefix."-lower");
              }
            }
            header('Location: http://sparvar.failar.nu');
            exit;
          }
        }
        ?>
        <!DOCTYPE HTML>
        <html>
          <head>
            <title>Sparvar player</title>
            <meta name="viewport" content="width=device-width, initial-scale=1.0" />
            <meta charset="UTF-8" />
            <style>
              body, html {
                background: black;
                color: white;
              }
              a {
                color: lightblue;
                font-weight: bold;
                padding: 10px;
                background: darkblue;
                text-decoration: none;
                border-radius: 8px;
                line-height: 50px;
              }
            </style>
          </head>
          <body>
            <h1>Sparv player</h1>
          <?php
            foreach ($validServicePrefixes as $service => $name) {
              echo "<a href='/?play=$service'>Play $name ▶️</a><br/>".PHP_EOL;
            }
            ?>
          </body>
        </html>
      '';
    in {
      root = "${testdir}/web";
      locations."~ \\.php$".extraConfig = ''
        fastcgi_pass unix:${config.services.phpfpm.pools.foobar.socket};
        fastcgi_index index.php;
        include ${config.services.nginx.package}/conf/fastcgi_params;
        include ${pkgs.nginx}/conf/fastcgi.conf;
      '';
      locations."/" = {
        tryFiles = "$uri $uri/ index.php";
        index = "index.php index.html index.htm";
      };
    };
  };
}
