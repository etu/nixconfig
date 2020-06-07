{ ... }: {
  # This file was populated at runtime with the networking
  # details gathered from the active system.
  networking = {
    useDHCP = false;
    nameservers = [
      "2001:4860:4860::8888"
      "2001:4860:4860::8844"
      "8.8.8.8"
      "8.8.4.4"
    ];
    defaultGateway = "172.31.1.1";
    defaultGateway6 = "fe80::1";
    interfaces = {
      eth0 = {
        ipv4 = {
          addresses = [
            { address = "116.203.101.178"; prefixLength = 32; }
          ];
          routes = [
            { address = "172.31.1.1"; prefixLength = 32; }
          ];
        };
        ipv6 = {
          addresses = [
            { address = "2a01:4f8:1c1c:d57f::1"; prefixLength = 64; }
          ];
          routes = [
            { address = "fe80::1"; prefixLength = 64; }
            { address = "::"; prefixLength = 0; via = "fe80::1"; }
          ];
        };
      };
    };
  };
  services.udev.extraRules = ''
    ATTR{address}=="96:00:00:1a:cb:d8", NAME="eth0"
  '';
}
