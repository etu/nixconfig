{ config, lib, ... }:

{
  imports = [
    ./freshrss
    ./jellyfin
    ./nfs
    ./syncthing
  ];
}
