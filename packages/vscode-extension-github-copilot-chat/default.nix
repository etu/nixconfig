# This file is automatically updated by the output from
# nix run github:etu/nixconfig#vscodeGetLatestExtensions github copilot-chat
#
# Store page: https://marketplace.visualstudio.com/items?itemName=github.copilot-chat
{ pkgs, ... }:
pkgs.vscode-utils.extensionFromVscodeMarketplace {
  publisher = "github";
  name = "copilot-chat";
  version = "0.48.1";
  sha256 = "sha256-eFLfYMFxvgtZtmwLsxfneMjD4jOg8/Uk0Eu/6+A6odY=";
}
