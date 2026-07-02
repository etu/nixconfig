# This file is automatically updated by the output from
# nix run github:etu/nixconfig#vscodeGetLatestExtensions Vue volar
#
# Store page: https://marketplace.visualstudio.com/items?itemName=Vue.volar
{ pkgs, ... }:
pkgs.vscode-utils.extensionFromVscodeMarketplace {
  publisher = "Vue";
  name = "volar";
  version = "3.3.6";
  sha256 = "sha256-FABzuJ7d4D2tIqGPilOvNdoAPxCVfn0dSWuQduixAUQ=";
}
