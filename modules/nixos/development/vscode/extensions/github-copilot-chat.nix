# This file is automatically updated by the output from
# nix run github:etu/nixconfig#vcodeGetLatestExtensions github copilot-chat
#
# Store page: https://marketplace.visualstudio.com/items?itemName=github.copilot-chat
{ pkgs, ... }:
pkgs.vscode-utils.extensionFromVscodeMarketplace {
  publisher = "github";
  name = "copilot-chat";
  version = "0.33.3";
  sha256 = "sha256-/I/RYM8Vnl4C8ZcFwAwTmNxqTXV4atQO7bOcuBz5+8g=";
}
