{
  config,
  pkgs,
  ...
}: {
  # Enable Klipper.
  services.klipper = {
    enable = true;

    # The user and group are set to moonraker, because moonraker
    # needs to access the socket of klipper... and klipper is
    # running as a dynamic user by default so I can't assign
    # moonraker to the same group as klipper.
    user = "moonraker";
    group = "moonraker";

    # Don't build the firmware, I have it flashed already.
    firmwares.mcu = {
      enable = true;
      configFile = ./klipper-firmware.cfg;
      # Serial port connected to the printer
      serial = "/dev/serial/by-id/usb-1a86_USB_Serial-if00-port0";
    };

    # These settings are based on this sample configuration file:
    # https://github.com/Klipper3d/klipper/blob/master/config/printer-creality-ender6-2020.cfg
    #
    # These settings have the BLTouch/CRTouch features enabled and
    # some extra settings needed for fluidd.
    settings = {
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
        z_offset = "1.8";
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
        rename_existing = "BASE_CANCEL_PRINT";
        gcode = "
          TURN_OFF_HEATERS
          CLEAR_PAUSE
          SDCARD_RESET_FILE
          BASE_CANCEL_PRINT
        ";
      };

      # To use this macro, we can use the following start print gcode in the slicer:
      #
      # > START_PRINT EXTRUDER_TEMP=[first_layer_temperature] BED_TEMP=[first_layer_bed_temperature]
      #
      # See docs:
      # https://github.com/Klipper3d/klipper/blob/daf875e6e4b8cb461a57623ecac37cf0f1f240e8/docs/Slicers.md#start_print-macros
      "gcode_macro START_PRINT".gcode = "
        {% set BED_TEMP = params.BED_TEMP|default(60)|float %}
        {% set EXTRUDER_TEMP = params.EXTRUDER_TEMP|default(210)|float %}

        G90                   ; use absolute coordinates
        M83                   ; extruder relative mode
        M140 S{BED_TEMP}      ; set final bed temp
        M104 S150             ; set temporary nozzle temp to prevent oozing during homing and auto bed leveling
        G4 S10                ; allow partial nozzle warmup
        M190 S{BED_TEMP}      ; wait for bed temp to stabilize

        G28                   ; home all axis
        BED_MESH_CALIBRATE    ; calibrate the bed

        G1 Z10 F240           ; lower bed 10mm
        G1 X2 Y10 F3000       ; move to front left corner
        M104 S{EXTRUDER_TEMP} ; set final nozzle temp
        M109 S{EXTRUDER_TEMP} ; wait for nozzle temp to stabilize
        G1 Z0.28 F240         ; Move close to bed
        G92 E0                ; reset Extruder
        G1 Y140 E10 F1500     ; draw first line
        G1 X2.3 F5000         ; move sideways to avoid blob
        G92 E0                ; reset Extruder
        G1 Y10 E10 F1200      ; draw second line
        G92 E0                ; reset Extruder
      ";

      # To use this macro, we can use the following end print gcode in the slicer:
      #
      # > END_PRINT
      #
      # See docs:
      # https://github.com/Klipper3d/klipper/blob/daf875e6e4b8cb461a57623ecac37cf0f1f240e8/docs/Slicers.md#start_print-macros
      "gcode_macro END_PRINT".gcode = "
        G91                   ; relative positioning
        G1 E-2 F2700          ; retract a bit
        G1 E-2 Z0.2 F2400     ; retract and raise Z
        G1 X5 Y5 F3000        ; wipe out
        G1 Z10                ; raise Z more
        G90                   ; absolute positioning

        G28 X Y               ; present print
        M107 S0               ; turn-off fan
        M104 S0               ; turn-off hotend
        M140 S0               ; turn-off heatbed

        M84 X Y E             ; disable all steppers but Z
      ";

      "gcode_macro M601".gcode = "
        SAVE_GCODE_STATE NAME=M601_state     ; save current print position for resume
        PAUSE                                ; pause print
        G91                                  ; relative positioning
        G1 E-2 F2700                         ; retract filament 2mm
        G1 Z10                               ; move bed down by 10mm
        G90                                  ; absolute positioning
        G28 X Y                              ; home X Y
        G91                                  ; relative positioning
        RESTORE_GCODE_STATE NAME=M601_state  ; restore position from previous state
      ";

      "gcode_macro M600".gcode = "
        SAVE_GCODE_STATE NAME=M600_state     ; save current print position for resume
        PAUSE                                ; pause print
        G91                                  ; relative positioning
        G1 E-2 F2700                         ; retract filament 2mm
        G1 Z10                               ; move bed down by 10mm
        G90                                  ; absolute positioning
        G28 X Y                              ; home X Y
        G91                                  ; relative positioning
        G1 E-50 F1000                        ; retract filament 50mm
        RESTORE_GCODE_STATE NAME=M600_state  ; restore position from previous state
      ";

      "gcode_macro FILAMENT_DRYER".gcode = "
        SET_IDLE_TIMEOUT IDLE_TIMEOUT=21600  ; Set idle timeout to 6 hours
        M190 S60                             ; Set bed temperature to 60C
      ";
    };
  };

  # Expose Klipper API's so they can be used.
  services.moonraker = {
    enable = true;

    # Configure moonraker.
    settings = {
      octoprint_compat = {};
      history = {};
      authorization = {
        force_logins = true;
        cors_domains = [
          "*://${config.networking.hostName}"
          "*://${config.networking.hostName}.tail1c46e.ts.net"
        ];
        trusted_clients = [
          "127.0.0.0/8"
        ];
      };

      # Static configuration of the camera to display in the UI.
      "webcam camera1" = {
        stream_url = "/klipper/webcam/?action=stream";
        snapshot_url = "/klipper/webcam/?action=snapshot";
      };
    };
  };

  # Enable Fluidd as a Web interface for Klipper via moonraker.
  services.nginx = {
    upstreams.fluidd-apiserver.servers = {
      "${config.services.moonraker.address}:${toString config.services.moonraker.port}" = {};
    };
    virtualHosts.${config.networking.hostName} = let
      fluidd-pkg = pkgs.runCommand "fluidd-pkg" {} ''
        mkdir $out
        ln -s ${config.services.fluidd.package}/share/fluidd/htdocs $out/klipper
      '';
    in {
      locations = {
        "/klipper" = {
          root = fluidd-pkg;
          extraConfig = "client_max_body_size 50M;";
          index = "index.html";
          tryFiles = "$uri $uri/ /index.html";
        };
        "/klipper/index.html" = {
          root = fluidd-pkg;
          extraConfig = ''
            client_max_body_size 50M;
            add_header Cache-Control "no-store, no-cache, must-revalidate";
          '';
        };
        # TODO: Move these to be under /klipper as path, this however requires
        # patching or maybe configuration of fluidd so it knows where to look.
        "/websocket" = {
          proxyWebsockets = true;
          extraConfig = "client_max_body_size 50M;";
          proxyPass = "http://fluidd-apiserver/websocket";
        };
        "~ ^/(printer|api|access|machine|server)/" = {
          proxyWebsockets = true;
          extraConfig = "client_max_body_size 50M;";
          proxyPass = "http://fluidd-apiserver$request_uri";
        };
        "/klipper/webcam".extraConfig = ''
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
      };
    };
  };

  # Enable mjpg-streamer.
  services.mjpg-streamer = {
    enable = true;
    # Lowest resulotion for better framerate.
    inputPlugin = "input_uvc.so -d /dev/v4l/by-id/usb-046d_0821_F8E393A0-video-index0 -r 640x480";
  };

  etu.base.zfs.system.directories = [
    # Bind mount for persistent data for moonraker
    "/var/lib/moonraker"
  ];
}
