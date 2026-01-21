# This file is automatically updated by the output from
# nix run github:etu/nixconfig#vcodeGetLatestExtensions github copilot-chat
#
# Store page: https://marketplace.visualstudio.com/items?itemName=github.copilot-chat
{ pkgs, ... }:
pkgs.vscode-utils.extensionFromVscodeMarketplace {
  publisher = "github";
  name = "copilot-chat";
  version = "0.36.1";
  sha256 = "sha256-fsiOf/4F7JWtKZPU+dpoZEf5v9wdceqCajRp9N0o+SU=";
}
