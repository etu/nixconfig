{ config, lib, ... }:

{
  imports = [
    ./nfs
    ./syncthing
    ./webos-devmode-keepalive
  ];
}
