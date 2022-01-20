{ config, ... }:

{
  # Enable syncthing
  services.syncthing.enable = true;
  services.syncthing.user = config.my.user.username;
  services.syncthing.group = "users";
  services.syncthing.dataDir = "/persistent/home/${config.my.user.username}";
  services.syncthing.devices = {
    "Elis Phone".id = "MDL3DI6-PZVLRSZ-ULYRM3X-AHLUBOA-T6QYYUW-U3Z6KV2-CFJDGWE-65WQFQ5";
    "Elis T495".id = "UPEWGIE-2OWALFY-REH7UKU-2Q6IZ5L-KJWCSUV-R7GNAYH-YPZAZ3C-CKNTIAU";
    "Elis T14s".id = "KFDW47A-EYYMH33-EDW3JRN-DAKY3UT-7JJOYNK-PLHY3CG-5IMTEJE-OJXRTQK";
  };
  services.syncthing.folders = {
    "${config.services.syncthing.dataDir}/.dotfiles/skeleton/org/" = {
      devices = [ "Elis Phone" "Elis T495" "Elis T14s" ];
      id = "v49fx-3yveu";
      label = "Org Files";
      versioning = {
        type = "simple";
        params.keep = "10";
      };
    };
  };
}
