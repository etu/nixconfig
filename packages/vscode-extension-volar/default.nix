# This file is automatically updated by the output from
# nix run github:etu/nixconfig#vscodeGetLatestExtensions Vue volar
#
# Store page: https://marketplace.visualstudio.com/items?itemName=Vue.volar
{ pkgs, ... }:
pkgs.vscode-utils.extensionFromVscodeMarketplace {
  publisher = "Vue";
  name = "volar";
  version = "3.3.5";
  sha256 = "sha256-Oz4or7AtfrlcAXmFB4lkNk4Uqmpm1AsGP4RLcdpzRbo=";
}
