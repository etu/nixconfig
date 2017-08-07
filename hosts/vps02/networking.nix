{ ... }:

{
  networking = {
    nameservers = [
      "2001:4860:4860::8888"
      "2001:4860:4860::8844"
      "8.8.8.8"
      "8.8.4.4"
    ];

    defaultGateway = "188.226.173.1";
    defaultGateway6 = "";
    interfaces = {
      eth0 = {
        ip4 = [
          { address="188.226.173.51"; prefixLength=24; }
          { address="10.14.0.5"; prefixLength=16; }
        ];
        ip6 = [
          { address="2a03:b0c0:0:1010::13d5:1"; prefixLength=64; }
          { address="fe80::8097:22ff:fea2:2958"; prefixLength=64; }
        ];
      };
    };
  };

  services.udev.extraRules = ''
    ATTR{address}=="82:97:22:a2:29:58", NAME="eth0"
  '';
}
