{
  moonraker,
  moonraker-timelapse,
  ...
}:
moonraker.overrideAttrs (oa: {
  pname = "moonraker-with-timelapse";

  installPhase =
    oa.installPhase
    + ''
      ln -s ${moonraker-timelapse}/lib/moonraker-timelapse/components/timelapse.py $out/lib/moonraker/components/timelapse.py
    '';
})
