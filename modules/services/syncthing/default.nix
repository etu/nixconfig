{ config, lib, ... }:

{
  options.etu.services.syncthing.enable = lib.mkEnableOption "Enable services syncthing settings";

  config = lib.mkIf config.etu.services.syncthing.enable {
    # Enable syncthing
    services.syncthing.enable = config.etu.user.enable;
    services.syncthing.user = config.etu.user.username;
    services.syncthing.group = "users";
    services.syncthing.dataDir = "/persistent/home/${config.etu.user.username}";

    # Build sync devices depending on hostname.
    services.syncthing.devices = {
    } // (lib.optionalAttrs (config.networking.hostName == "fenchurch") {
      "phone".id = "MDL3DI6-PZVLRSZ-ULYRM3X-AHLUBOA-T6QYYUW-U3Z6KV2-CFJDGWE-65WQFQ5";
      "agrajag".id = "UPEWGIE-2OWALFY-REH7UKU-2Q6IZ5L-KJWCSUV-R7GNAYH-YPZAZ3C-CKNTIAU";
      "eliaxe-A100514-NR".id = "KFDW47A-EYYMH33-EDW3JRN-DAKY3UT-7JJOYNK-PLHY3CG-5IMTEJE-OJXRTQK";
    }) // (lib.optionalAttrs (config.networking.hostName == "agrajag" || config.networking.hostName == "eliaxe-A100514-NR") {
      "fenchurch".id = "7B35F77-T3Z5G4M-NB7BNZD-2RFYRLX-7ICVUYT-6CCPW3S-OUKXW4W-UCJQ2QZ";
    });

    services.syncthing.folders = {
      "${config.services.syncthing.dataDir}/.dotfiles/skeleton/org/" = {
        # Build devices for this folder depending on hostname.
        devices = [
        ] ++ (lib.optionals (config.networking.hostName == "fenchurch") [
          "phone"
          "agrajag"
          "eliaxe-A100514-NR"
        ]) ++ (lib.optionals (config.networking.hostName == "agrajag" || config.networking.hostName == "eliaxe-A100514-NR") [
          "fenchurch"
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
