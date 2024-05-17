{
  lib,
  stdenvNoCC,
  fetchFromGitHub,
  ffmpeg,
  wget,
}: let
  pname = "moonraker-timelapse";
in
  stdenvNoCC.mkDerivation {
    inherit pname;
    version = "unstable-2023-12-16";

    src = fetchFromGitHub {
      owner = "mainsail-crew";
      repo = pname;
      rev = "c7fff11e542b95e0e15b8bb1443cea8159ac0274";
      hash = "sha256-ZYSeSn3OTManyTbNOnCfhormjFMgomNk3VXOVqBr9zg=";
    };

    buildInputs = [
      ffmpeg
      wget
    ];

    dontBuild = true;

    installPhase = ''
      mkdir -p $out/lib/moonraker-timelapse/{components,klipper-macro}/

      # Copy the timelapse component for easy use from the
      # moonraker package.
      cp component/timelapse.py $out/lib/moonraker-timelapse/components/

      # Substitute in ffmpeg
      substituteInPlace $out/lib/moonraker-timelapse/components/timelapse.py \
        --replace /usr/bin/ffmpeg ${ffmpeg}/bin/ffmpeg \
        --replace 'wget ' '${wget}/bin/wget '

      # Copy the klipper macro for easy use from the klipper package.
      cp klipper_macro/timelapse.cfg $out/lib/moonraker-timelapse/klipper-macro/
    '';

    meta = {
      description = "Timelapse Plugin for moonraker";
      homepage = "https://github.com/mainsail-crew/moonraker-timelapse";
      license = lib.licenses.gpl3Only;
      maintainers = with lib.maintainers; [etu];
    };
  }
