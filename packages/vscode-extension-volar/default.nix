# This file is automatically updated by the output from
# nix run github:etu/nixconfig#vscodeGetLatestExtensions Vue volar
#
# Store page: https://marketplace.visualstudio.com/items?itemName=Vue.volar
{ pkgs, ... }:
pkgs.vscode-utils.extensionFromVscodeMarketplace {
  publisher = "Vue";
  name = "volar";
  version = "3.2.7";
  sha256 = "sha256-KLPb4XTm1lD44D4ajdH1Gr0J0JaN5TpaGp+bCpSuo3U=";
}
