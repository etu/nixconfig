{
  config,
  lib,
  ...
}: {
  options.etu.services.syncthing.enable = lib.mkEnableOption "Enable services syncthing settings";

  config = lib.mkIf config.etu.services.syncthing.enable {
    # Enable syncthing
    services.syncthing.enable = config.etu.user.enable;
    services.syncthing.user = config.etu.user.username;
    services.syncthing.group = "users";
    services.syncthing.dataDir = "${config.etu.dataPrefix}/home/${config.etu.user.username}";

    # Build sync devices depending on hostname.
    services.syncthing.settings.devices =
      {}
      // (lib.optionalAttrs (config.networking.hostName == "server-main-elis") {
        "phone".id = "6LAUOGD-LGAXEGF-NY56V6I-UYK5TSD-JBF736P-LUGZ32K-AFUAUOT-PRXPTAL";
        "laptop-private-elis".id = "UPEWGIE-2OWALFY-REH7UKU-2Q6IZ5L-KJWCSUV-R7GNAYH-YPZAZ3C-CKNTIAU";
        "laptop-work-elis".id = "KFDW47A-EYYMH33-EDW3JRN-DAKY3UT-7JJOYNK-PLHY3CG-5IMTEJE-OJXRTQK";
      })
      // (lib.optionalAttrs (config.networking.hostName == "laptop-private-elis" || config.networking.hostName == "laptop-work-elis") {
        "server-main-elis".id = "7B35F77-T3Z5G4M-NB7BNZD-2RFYRLX-7ICVUYT-6CCPW3S-OUKXW4W-UCJQ2QZ";
      });

    services.syncthing.settings.folders = {
      "${config.services.syncthing.dataDir}/.dotfiles/skeleton/org/" = {
        # Build devices for this folder depending on hostname.
        devices =
          (lib.optionals (config.networking.hostName == "server-main-elis") [
            "phone"
            "laptop-private-elis"
            "laptop-work-elis"
          ])
          ++ (lib.optionals (config.networking.hostName == "laptop-private-elis" || config.networking.hostName == "laptop-work-elis") [
            "server-main-elis"
          ]);

        id = "v49fx-3yveu";
        label = "Org Files";
        versioning = {
          type = "simple";
          params.keep = "10";
        };
      };
    };
  };
}
