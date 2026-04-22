# This file is automatically updated by the output from
# nix run github:etu/nixconfig#vscodeGetLatestExtensions github copilot-chat
#
# Store page: https://marketplace.visualstudio.com/items?itemName=github.copilot-chat
{ pkgs, ... }:
pkgs.vscode-utils.extensionFromVscodeMarketplace {
  publisher = "github";
  name = "copilot-chat";
  version = "0.44.2";
  sha256 = "sha256-kjLpbA6zUta4K86yEDiLNWvy3kJ3AvF2fncCO/JVl6I=";
}
