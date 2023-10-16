{pkgs, ...}: {
  systemd.services.media-empty-dirs-cleaner = {
    description = "Remove empty directories from /media/zstorage/files/{audio,video}";
    wantedBy = ["multi-user.target"];
    startAt = "hourly";
    serviceConfig = {
      Type = "oneshot";
      ExecStart = pkgs.writeScript "media-empty-dirs-cleaner" ''
        #!${pkgs.bash}/bin/bash
        set -euo pipefail

        ${pkgs.findutils}/bin/find /media/zstorage/files/audio /media/zstorage/files/video \
          -type d -empty -print -delete
      '';
    };
  };
}
