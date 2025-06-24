{pkgs, ...}:
pkgs.vscode-utils.extensionFromVscodeMarketplace {
  # https://marketplace.visualstudio.com/items?itemName=Vue.volar
  publisher = "Vue";
  name = "volar";
  version = "1.8.27";
  sha256 = "sha256-KfWgdz61NURmS1cwFsE9AmIrEykyN5MXIKfG8gDfmac=";
}
