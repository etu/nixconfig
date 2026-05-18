# This file is automatically updated by the output from
# nix run github:etu/nixconfig#vscodeGetLatestExtensions github copilot-chat
#
# Store page: https://marketplace.visualstudio.com/items?itemName=github.copilot-chat
{ pkgs, ... }:
pkgs.vscode-utils.extensionFromVscodeMarketplace {
  publisher = "github";
  name = "copilot-chat";
  version = "0.45.1";
  sha256 = "sha256-xxJ+h0/XyT8otXUzIYW9/KMxKLk5zoEE/fiqj4SZK+A=";
}
