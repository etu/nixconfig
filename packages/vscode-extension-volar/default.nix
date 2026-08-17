# This file is automatically updated by the output from
# nix run github:etu/nixconfig#vscodeGetLatestExtensions Vue volar
#
# Store page: https://marketplace.visualstudio.com/items?itemName=Vue.volar
{ pkgs, ... }:
pkgs.vscode-utils.extensionFromVscodeMarketplace {
  publisher = "Vue";
  name = "volar";
  version = "3.3.10";
  sha256 = "sha256-Xo00acG30S1/XvCyZXizU7dp64hC/fBtS5F/yDQWc20=";
}
