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
    defaultGateway = { address = "172.31.1.1"; interface = "eth0"; };
    defaultGateway6 = { address = "fe80::1"; interface = "eth0"; };
    interfaces.eth0 = {
      ipv4.addresses = [
        { address = "49.12.203.105"; prefixLength = 32; }
      ];
      ipv4.routes = [
        { address = "172.31.1.1"; prefixLength = 32; }
      ];
      ipv6.addresses = [
        { address = "2a01:4f8:c0c:8d7b::1"; prefixLength = 64; }
      ];
    };
  };
  services.udev.extraRules = ''
    ATTR{address}=="96:00:01:99:e6:ee", NAME="eth0"
  '';
}
