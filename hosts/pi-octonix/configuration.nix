# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{modulesPath, ...}: {
  imports = [
    # Import bootloader and related settings for aarch64
    (modulesPath + "/installer/sd-card/sd-image-aarch64.nix")
  ];

  # Don't compress the resulting image.
  sdImage.compressImage = false;

  # Disable documentation to make the system smaller.
  documentation.enable = false;
  documentation.doc.enable = false;
  documentation.info.enable = false;
  documentation.man.enable = false;

  # Set hostname for system.
  networking.hostName = "octonix";

  # My module settings
  etu = {
    stateVersion = "24.05";

    # Set data prefix so agenix can find the host keys.
    dataPrefix = "/";

    # Disable ZFS helpers to avoid persistence weirdness.
    base.zfs.enable = false;

    # Disable Emacs to save some space, won't be used anyways.
    base.emacs.enable = false;

    # Don't set a password for root depending on agenix.
    user.setEmptyRootPassword = true;
  };

  # Allow root to log in without password.
  users.users.root.initialHashedPassword = "";

  # Automatically log in at the virtual consoles.
  services.getty.autologinUser = "root";

  # Wifi.
  hardware.enableRedistributableFirmware = true;

  # Enable wireless networking.
  networking.wireless.enable = true;
  networking.wireless.interfaces = ["wlan0"];
  networking.wireless.networks."SSID".psk = "PASSWORD"; # Secrets

  # Enable Klipper.
  services.klipper = {
    enable = true;
    # Enable building firmware
    firmwares.mcu = {
      enable = true;
      configFile = ./klipper-firmware.cfg;
      # Serial port connected to the printer
      serial = "/dev/serial/by-id/usb-1a86_USB_Serial-if00-port0";
    };
    settings = {
      # These settings are based on this sample configuration file:
      # https://github.com/Klipper3d/klipper/blob/master/config/printer-creality-ender6-2020.cfg
      #
      # These settings have the BLTouch/CRTouch features enabled and
      # some extra settings needed for fluidd.
      stepper_x = {
        step_pin = "PB8";
        dir_pin = "PB7";
        enable_pin = "!PC3";
        microsteps = 16;
        rotation_distance = 40;
        endstop_pin = "^PA5";
        position_endstop = 260;
        position_max = 260;
        homing_speed = 50;
      };

      stepper_y = {
        step_pin = "PC2";
        dir_pin = "!PB9";
        enable_pin = "!PC3";
        microsteps = 16;
        rotation_distance = 40;
        endstop_pin = "^PA6";
        position_endstop = 260;
        position_max = 260;
        homing_speed = 50;
      };

      stepper_z = {
        step_pin = "PB6";
        dir_pin = "PB5";
        enable_pin = "!PC3";
        microsteps = 16;
        rotation_distance = 8;
        endstop_pin = "probe:z_virtual_endstop";
        position_min = "-5";
        position_max = 400;
      };

      safe_z_home = {
        home_xy_position = "150.7, 137";
        speed = 100;
        z_hop = 10;
        z_hop_speed = 5;
      };

      bltouch = {
        sensor_pin = "^PB1";
        control_pin = "PB0";
        x_offset = "-20.7";
        y_offset = "-7";
        # 2.4 was suggested preset for bltouch, but I have a crtouch
        # so it may differ I guess. This works well for me. ¯\_(ツ)_/¯
        z_offset = "1.9";
        speed = "3.0";
      };

      bed_mesh = {
        speed = 100;
        mesh_min = "10, 10";
        mesh_max = "239, 239";
        algorithm = "bicubic";
        probe_count = "5, 5";
      };

      extruder = {
        max_extrude_only_distance = "1000.0";
        step_pin = "PB4";
        dir_pin = "!PB3";
        enable_pin = "!PC3";
        microsteps = 16;
        rotation_distance = "22.857";
        nozzle_diameter = "0.400";
        filament_diameter = "1.750";
        heater_pin = "PA1";
        sensor_type = "EPCOS 100K B57560G104F";
        sensor_pin = "PC5";
        control = "pid";
        pid_Kp = "26.949";
        pid_Ki = "1.497";
        pid_Kd = "121.269";
        min_temp = 0;
        max_temp = 260;
      };

      heater_bed = {
        heater_pin = "PA2";
        sensor_type = "EPCOS 100K B57560G104F";
        sensor_pin = "PC4";
        control = "pid";
        pid_Kp = "327.11";
        pid_Ki = "19.20";
        pid_Kd = "1393.45";
        min_temp = 0;
        max_temp = 100;
      };

      fan.pin = "PA0";

      "filament_switch_sensor e0_sensor".switch_pin = "PA4";

      mcu = {
        serial = "/dev/serial/by-id/usb-1a86_USB_Serial-if00-port0";
        restart_method = "command";
      };

      printer = {
        kinematics = "corexy";
        max_velocity = 500;
        max_accel = 2000;
        max_z_velocity = 10;
        max_z_accel = 100;
      };

      # These are aproximate locations for my bed screws to assist in
      # manual bedlevling.
      screws_tilt_adjust = {
        # Possibly exact screw positions, offsetted by tool position.
        screw1 = "54.7, 43"; # Position for printer head: X: 34, Y: 36
        screw1_name = "front left";
        screw2 = "244.7, 43"; # Position for printer head: X: 224, Y: 36
        screw2_name = "front right";
        screw3 = "54.7, 235"; # Position for printer head: X: 34, Y: 228
        screw3_name = "back left";
        screw4 = "244.7, 235"; # Position for printer head: X: 224, Y: 228
        screw4_name = "back right";
      };

      # Required settings for fluidd to work properly:
      #
      # Storage path for uploaded files.
      virtual_sdcard.path = "/var/lib/moonraker/gcodes";

      # Other things fluidd needs to exist:
      display_status = {};
      pause_resume = {};

      "gcode_macro POWEROFF".gcode = "
        RESPOND TYPE=command MSG=action:poweroff
      ";

      "gcode_macro CANCEL_PRINT" = {
        description = "Cancel the actual running print";
        rename_existing = "CANCEL_PRINT_BASE";
        gcode = "
          TURN_OFF_HEATERS
          CLEAR_PAUSE
          SDCARD_RESET_FILE
          BASE_CANCEL_PRINT
        ";
      };
    };
  };

  # Polkit to make moonraker happy.
  security.polkit.enable = true;

  # Expose Klipper API's so they can be used.
  services.moonraker = {
    enable = true;
    # user moonraker doesn't have access to the socket owned by
    # klipper:klipper. There's got to be a better way.
    user = "root";
    allowSystemControl = true; # Adds polkit rules to manage the system.
    settings = {
      octoprint_compat = {};
      history = {};
      authorization = {
        force_logins = true;
        cors_domains = [
          "*.local"
          "*.lan"
          "*://app.fluidd.xyz"
          "*://my.mainsail.xyz"
          "*://octonix"
          "*://octonix.tail1c46e.ts.net"
        ];
        trusted_clients = [
          "10.0.0.0/8"
          "100.64.0.0/10"
          "127.0.0.0/8"
          "169.254.0.0/16"
          "172.16.0.0/12"
          "192.168.1.0/24"
          "FE80::/10"
          "::1/128"
        ];
      };

      # Static configuration of the camera to display in the UI.
      "webcam camera1" = {
        stream_url = "/webcam/?action=stream";
        snapshot_url = "/webcam/?action=snapshot";
      };
    };
  };

  # Enable Fluidd as a Web interface for Klipper via moonraker.
  services.fluidd.enable = true;
  services.nginx.clientMaxBodySize = "50m"; # Increase max file upload size from 10m

  # Expose the ustreamer stream through nginx.
  services.fluidd.nginx.locations."/webcam".extraConfig = ''
    set $pp_d http://127.0.0.1:5050/stream_simple.html;

    if ( $args ~ '.*action=stream.*' ) {
      set $pp_d http://127.0.0.1:5050/$is_args$args;
    }

    if ( $args ~ '.*action=snapshot.*' ) {
      set $pp_d http://127.0.0.1:5050/$is_args$args;
    }

    proxy_pass $pp_d;
    proxy_http_version 1.1;
    proxy_set_header Upgrade $http_upgrade;
    proxy_set_header Connection "upgrade";
    proxy_set_header Host $host:$server_port;
    proxy_set_header X-Forwarded-Proto $scheme;
    proxy_set_header X-Forwarded-For $remote_addr;
    proxy_set_header X-Forwarded-Port $server_port;
    proxy_set_header X-Request-Start $msec;
  '';

  # Enable mjpg streamer.
  services.mjpg-streamer = {
    enable = true;
    # Lowest resulotion for better framerate.
    inputPlugin = "input_uvc.so -d /dev/video0 -r 640x480";
  };

  # Open port for nginx.
  networking.firewall.allowedTCPPorts = [80];
}
