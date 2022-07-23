{ config, lib, ... }:

{
  imports = [
    ./jellyfin
    ./nfs
    ./syncthing
  ];
}
