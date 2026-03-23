# This file is automatically updated by the output from
# nix run github:etu/nixconfig#vscodeGetLatestExtensions github copilot-chat
#
# Store page: https://marketplace.visualstudio.com/items?itemName=github.copilot-chat
{ pkgs, ... }:
pkgs.vscode-utils.extensionFromVscodeMarketplace {
  publisher = "github";
  name = "copilot-chat";
  version = "0.40.1";
  sha256 = "sha256-wVxryUaW53xU4zZKU4t1pZaAB7BFIOkkcng91JnCLOk=";
}
