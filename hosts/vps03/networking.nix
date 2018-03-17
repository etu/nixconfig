{ ... }: {
  # This file was populated at runtime with the networking
  # details gathered from the active system.
  networking = {
    nameservers = [
      "2001:4860:4860::8888"
      "2001:4860:4860::8844"
      "8.8.8.8"
      "8.8.4.4"
    ];
    defaultGateway = "146.185.156.1";
    defaultGateway6 = "2a03:b0c0:0:1010::1";
    interfaces = {
      eth0 = {
        ipv4.addresses = [
          { address="146.185.156.9"; prefixLength=24; }
          { address="10.14.0.6"; prefixLength=16; }
        ];
        ipv6.addresses = [
          { address="2a03:b0c0:0:1010::23a:4001"; prefixLength=64; }
          { address="fe80::787f:b7ff:fe4e:edfd"; prefixLength=64; }
        ];
      };
      
    };
  };
  services.udev.extraRules = ''
    ATTR{address}=="7a:7f:b7:4e:ed:fd", NAME="eth0"
  '';
}
