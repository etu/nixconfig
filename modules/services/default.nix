{
  config,
  lib,
  ...
}: {
  imports = [
    ./freshrss
    ./jellyfin
    ./netdata
    ./nfs
    ./syncthing
  ];
}
