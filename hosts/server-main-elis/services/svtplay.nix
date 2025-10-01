{ pkgs, ... }:
let
  buildSvtPlayService = svtSlug: {
    description = "${svtSlug} playlist updater";
    wants = [ "network-online.target" ];
    after = [ "network-online.target" ];
    path = [ pkgs.svtplay-dl ];
    script = "svtplay-dl --all-episodes --get-url --preferred DASH https://www.svtplay.se/${svtSlug} > ${svtSlug}.m3u";
    serviceConfig = {
      Type = "simple";
      User = "downloads";
      Group = "downloads";
      WorkingDirectory = "/data/var/www/misc.elis.nu/.svtplay";
    };
  };
  buildSvtPlayTimer = svtSlug: {
    description = "${svtSlug} playlist updater timer";
    after = [ "network-online.target" ];
    wants = [ "network-online.target" ];
    wantedBy = [ "timers.target" ];
    timerConfig.OnCalendar = "daily";
  };
in
{
  systemd.services.svtplay-faret-shaun = buildSvtPlayService "faret-shaun";
  systemd.timers.svtplay-faret-shaun = buildSvtPlayTimer "faret-shaun";

  systemd.services.svtplay-mamma-mu = buildSvtPlayService "mamma-mu";
  systemd.timers.svtplay-mamma-mu = buildSvtPlayTimer "mamma-mu";

  systemd.services.svtplay-pettson-och-findus = buildSvtPlayService "pettson-och-findus";
  systemd.timers.svtplay-pettson-och-findus = buildSvtPlayTimer "pettson-och-findus";

  systemd.services.svtplay-filtis-och-fluff = buildSvtPlayService "filtis-och-fluff";
  systemd.timers.svtplay-filtis-och-fluff = buildSvtPlayTimer "filtis-och-fluff";

  systemd.services.svtplay-greta-gris = buildSvtPlayService "greta-gris";
  systemd.timers.svtplay-greta-gris = buildSvtPlayTimer "greta-gris";

  systemd.services.svtplay-minibods = buildSvtPlayService "minibods";
  systemd.timers.svtplay-minibods = buildSvtPlayTimer "minibods";
}
