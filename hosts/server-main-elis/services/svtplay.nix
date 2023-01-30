{
  config,
  pkgs,
  lib,
  ...
}: let
  buildSvtPlayService = svtSlug: {
    description = "${svtSlug} updater";
    after = ["network-online.target"];
    wantedBy = ["multi-user.target"];
    startAt = "04:00";
    path = [pkgs.ffmpeg];
    preStart = "mkdir -p ${svtSlug}";
    serviceConfig = {
      Type = "oneshot";
      User = "downloads";
      Group = "downloads";
      WorkingDirectory = "/media/zstorage/files/video/svt-series";
      ExecStart = "${pkgs.svtplay-dl}/bin/svtplay-dl --nfo --all-episodes --output ${svtSlug} https://www.svtplay.se/${svtSlug}";
    };
  };
in {
  #systemd.services.svtplay-faret-shaun = buildSvtPlayService "faret-shaun";
  #systemd.services.svtplay-mamma-mu = buildSvtPlayService "mamma-mu";
  #systemd.services.svtplay-pettson-och-findus = buildSvtPlayService "pettson-och-findus";
}
