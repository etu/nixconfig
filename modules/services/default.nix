{ config, lib, ... }:

{
  imports = [
    ./jellyfin
    ./nfs
    ./syncthing
    ./webos-devmode-keepalive
  ];
}
