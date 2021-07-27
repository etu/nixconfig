{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.services.mmdvmhost;
  settingsFormat = pkgs.formats.ini { };

  arduipi_oled = pkgs.callPackage ./deps/arduipi_oled.nix { };
  mmdvmhost = pkgs.callPackage ./package.nix {
    inherit arduipi_oled;
  };
in
{
  options.services.mmdvmhost = {
    enable = lib.mkEnableOption "MMDVMHost";
    settings = lib.mkOption {
      inherit (settingsFormat) type;
      default = { };
      description = ''
        Override settings from the default configuration file.

        We set the (most of the) default settings from
        <link xlink:href="https://github.com/g4klx/MMDVMHost/blob/3f65200d7d19589dfc4c420d65c24686a6eaf4d4/MMDVM.ini"/>, however, we disable all modes by default so they need to be enabled.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    services.mmdvmhost.settings = {
      General = {
        Callsign = lib.mkDefault "G9BF";
        Id = lib.mkDefault 123456;
        Timeout = lib.mkDefault 180;
        Duplex = lib.mkDefault 1;
        RFModeHang = lib.mkDefault 10;
        NetModeHang = lib.mkDefault 3;
        Display = lib.mkDefault "None";
        Daemon = lib.mkDefault 0;
      };
      Info = {
        RXFrequency = lib.mkDefault 435000000;
        TXFrequency = lib.mkDefault 435000000;
        Power = lib.mkDefault 1;
        Latitude = lib.mkDefault 0.0;
        Longitude = lib.mkDefault 0.0;
        Height = lib.mkDefault 0;
        Location = lib.mkDefault "Nowhere";
        Description = lib.mkDefault "Multi-Mode Repeater";
        URL = lib.mkDefault "example.org";
      };
      Log = {
        DisplayLevel = lib.mkDefault 1;
        FileLevel = lib.mkDefault 1;
        FilePath = "/var/log/mmdvmhost";
        FileRoot = "mmdvmhost";
        FileRotate = 1;
      };
      "CW Id" = {
        Enable = lib.mkDefault 0;
        Time = lib.mkDefault 10;
      };
      "DMR Id Lookup" = {
        File = "DMRIds.dat"; # TODO: Download and specify a DMRIds.dat file
        Time = 24;
      };
      "NXDN Id Lookup" = {
        File = "NXDN.csv"; # TODO: Download and specify a NXDN.csv file
        Time = 24;
      };
      Modem = {
        Port = lib.mkDefault "/dev/ttyACM0";
        Protocol = lib.mkDefault "uart";
        TXInvert = lib.mkDefault 1;
        RXInvert = lib.mkDefault 0;
        PTTInvert = lib.mkDefault 0;
        TXDelay = lib.mkDefault 100;
        RXOffset = lib.mkDefault 0;
        TXOffset = lib.mkDefault 0;
        DMRDelay = lib.mkDefault 0;
        RXLevel = lib.mkDefault 50;
        TXLevel = lib.mkDefault 50;
        RXDCOffset = lib.mkDefault 0;
        TXDCOffset = lib.mkDefault 0;
        RFLevel = lib.mkDefault 100;
        RSSIMappingFile = "RSSI.dat"; # TODO: Download and specify a RSSI.csv file
        UseCOSAsLockout = lib.mkDefault 0;
        Trace = lib.mkDefault 0;
        Debug = lib.mkDefault 0;
      };
      "Transparent Data" = {
        Enable = lib.mkDefault 0;
        RemoteAddress = lib.mkDefault "127.0.0.1";
        RemotePort = lib.mkDefault 40094;
        LocalPort = lib.mkDefault 40095;
      };
      UMP = {
        Enable = lib.mkDefault 0;
        Port = lib.mkDefault "/dev/ttyACM1";
      };
      D-Star = {
        Enable = lib.mkDefault 0;
        Module = lib.mkDefault "C";
        SelfOnly = lib.mkDefault 0;
        AckReply = lib.mkDefault 1;
        AckTime = lib.mkDefault 750;
        AckMessage = lib.mkDefault 0;
        ErrorReply = lib.mkDefault 1;
        RemoteGateway = lib.mkDefault 0;
        WhiteList = lib.mkDefault "";
      };
      DMR = {
        Enable = lib.mkDefault 0;
        Beacons = lib.mkDefault 0;
        BeaconInterval = lib.mkDefault 60;
        BeaconDuration = lib.mkDefault 3;
        ColorCode = lib.mkDefault 1;
        SelfOnly = lib.mkDefault 0;
        EmbeddedLCOnly = lib.mkDefault 0;
        DumpTAData = lib.mkDefault 1;
        CallHang = lib.mkDefault 3;
        TXHang = lib.mkDefault 4;
        ModeHang = lib.mkDefault 30;
        OVCM = lib.mkDefault 0;
        Id = lib.mkDefault 123456;
      };
      "System Fusion" = {
        Enable = lib.mkDefault 0;
        LowDeviation = lib.mkDefault 0;
        SelfOnly = lib.mkDefault 0;
        TXHang = lib.mkDefault 4;
        RemoteGateway = lib.mkDefault 0;
      };
      P25 = {
        Enable = lib.mkDefault 0;
        NAC = lib.mkDefault 293;
        SelfOnly = lib.mkDefault 0;
        OverrideUIDCheck = lib.mkDefault 0;
        RemoteGateway = lib.mkDefault 0;
        TXHang = lib.mkDefault 5;
      };
      NXDN = {
        Enable = lib.mkDefault 0;
        RAN = lib.mkDefault 1;
        SelfOnly = lib.mkDefault 0;
        RemoteGateway = lib.mkDefault 0;
        TXHang = lib.mkDefault 5;
      };
      POCSAG = {
        Enable = lib.mkDefault 0;
        Frequency = lib.mkDefault 439987500;
      };
      FM = {
        Enable = lib.mkDefault 0;
        Callsign = lib.mkDefault "G4KLX";
        CallsignSpeed = lib.mkDefault 20;
        CallsignFrequency = lib.mkDefault 1000;
        CallsignTime = lib.mkDefault 10;
        CallsignHoldoff = lib.mkDefault 0;
        CallsignHighLevel = lib.mkDefault 50;
        CallsignLowLevel = lib.mkDefault 20;
        CallsignAtStart = lib.mkDefault 1;
        CallsignAtEnd = lib.mkDefault 1;
        CallsignAtLatch = lib.mkDefault 0;
        RFAck = lib.mkDefault "K";
        ExtAck = lib.mkDefault "N";
        AckSpeed = lib.mkDefault 20;
        AckFrequency = lib.mkDefault 1750;
        AckMinTime = lib.mkDefault 4;
        AckDelay = lib.mkDefault 1000;
        AckLevel = lib.mkDefault 50;
        Timeout = lib.mkDefault 180;
        TimeoutLevel = lib.mkDefault 80;
        CTCSSFrequency = lib.mkDefault 88.4;
        CTCSSThreshold = lib.mkDefault 30;
        CTCSSHighThreshold = lib.mkDefault 30;
        CTCSSLowThreshold = lib.mkDefault 20;
        CTCSSLevel = lib.mkDefault 20;
        KerchunkTime = lib.mkDefault 0;
        HangTime = lib.mkDefault 7;
        AccessMode = lib.mkDefault 1;
        COSInvert = lib.mkDefault 0;
        RFAudioBoost = lib.mkDefault 1;
        MaxDevLevel = lib.mkDefault 90;
        ExtAudioBoost = lib.mkDefault 1;
      };
      "D-Star Network" = {
        Enable = lib.mkDefault 0;
        GatewayAddress = lib.mkDefault "127.0.0.1";
        GatewayPort = lib.mkDefault 20010;
        LocalPort = lib.mkDefault 20011;
        Debug = lib.mkDefault 0;
      };
      "DMR Network" = {
        Enable = lib.mkDefault 0;
        Type = lib.mkDefault "Gateway";
        Address = lib.mkDefault "127.0.0.1";
        Port = lib.mkDefault 62031;
        Local = lib.mkDefault 62032;
        Password = lib.mkDefault "P@ssw0rd1234";
        Jitter = lib.mkDefault 360;
        Slot1 = lib.mkDefault 1;
        Slot2 = lib.mkDefault 1;
        Debug = lib.mkDefault 0;
      };
      "System Fusion Network" = {
        Enable = lib.mkDefault 0;
        LocalAddress = lib.mkDefault "127.0.0.1";
        LocalPort = lib.mkDefault 3200;
        GatewayAddress = lib.mkDefault "127.0.0.1";
        GatewayPort = lib.mkDefault 4200;
        Debug = lib.mkDefault 0;
      };
      "P25 Network" = {
        Enable = lib.mkDefault 0;
        GatewayAddress = lib.mkDefault "127.0.0.1";
        GatewayPort = lib.mkDefault 42020;
        LocalPort = lib.mkDefault 32010;
        Debug = lib.mkDefault 0;
      };
      "NXDN Network" = {
        Enable = lib.mkDefault 0;
        Protocol = lib.mkDefault "Icom";
        LocalAddress = lib.mkDefault "127.0.0.1";
        LocalPort = lib.mkDefault 14021;
        GatewayAddress = lib.mkDefault "127.0.0.1";
        GatewayPort = lib.mkDefault 14020;
        Debug = lib.mkDefault 0;
      };
      "POCSAG Network" = {
        Enable = lib.mkDefault 0;
        LocalAddress = lib.mkDefault "127.0.0.1";
        LocalPort = lib.mkDefault 3800;
        GatewayAddress = lib.mkDefault "127.0.0.1";
        GatewayPort = lib.mkDefault 4800;
        Debug = lib.mkDefault 0;
      };
      "TFT Serial" = {
        Port = lib.mkDefault "/dev/ttyAMA0";
        Brightness = lib.mkDefault 50;
      };
      "HD44780" = {
        Rows = lib.mkDefault 2;
        Columns = lib.mkDefault 16;
        Pins = lib.mkDefault "11,10,0,1,2,3";
        I2CAddress = lib.mkDefault "0x20";
        PWM = lib.mkDefault 0;
        PWMPin = lib.mkDefault 21;
        PWMBright = lib.mkDefault 100;
        PWMDim = lib.mkDefault 16;
        DisplayClock = lib.mkDefault 1;
        UTC = lib.mkDefault 0;
      };
      "Nextion" = {
        Port = lib.mkDefault "/dev/ttyAMA0";
        Brightness = lib.mkDefault 50;
        DisplayClock = lib.mkDefault 1;
        UTC = lib.mkDefault 0;
        ScreenLayout = lib.mkDefault 2;
        IdleBrightness = lib.mkDefault 20;
      };
      OLED = {
        Type = lib.mkDefault 3;
        Brightness = lib.mkDefault 0;
        Invert = lib.mkDefault 0;
        Scroll = lib.mkDefault 1;
        Rotate = lib.mkDefault 0;
        Cast = lib.mkDefault 0;
        LogoScreensaver = lib.mkDefault 1;
      };
      LCDproc = {
        Address = lib.mkDefault "localhost";
        Port = lib.mkDefault 13666;
        LocalPort = lib.mkDefault 13667;
        DimOnIdle = lib.mkDefault 0;
        DisplayClock = lib.mkDefault 1;
        UTC = lib.mkDefault 0;
      };
      "Lock File" = {
        Enable = lib.mkDefault 0;
        File = "/tmp/MMDVM_Active.lck";
      };
      "Remote Control" = {
        Enable = lib.mkDefault 0;
        Address = lib.mkDefault "127.0.0.1";
        Port = lib.mkDefault 7642;
      };
    };

    users.users.mmdvm = {
      isSystemUser = true;
      description = "mmdvm";
      group = "mmdvm";
      extraGroups = [ "dialout" ];
    };
    users.groups.mmdvm = { };

    systemd.services.mmdvmhost = {
      description = "MMDVMHost";
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        User = "mmdvm";
        ExecStart = "${mmdvmhost}/bin/MMDVMHost ${settingsFormat.generate "mmdvmhost-config.ini" cfg.settings}";
        Restart = "always";
        RestartSec = "5s";
        LogsDirectory = "mmdvmhost";
        LogsDirectoryMode = "0755";
      };
    };
  };
}
