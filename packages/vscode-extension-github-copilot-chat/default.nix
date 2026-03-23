# This file is automatically updated by the output from
# nix run github:etu/nixconfig#vscodeGetLatestExtensions github copilot-chat
#
# Store page: https://marketplace.visualstudio.com/items?itemName=github.copilot-chat
{ pkgs, ... }:
pkgs.vscode-utils.extensionFromVscodeMarketplace {
  publisher = "github";
  name = "copilot-chat";
  version = "0.39.2";
  sha256 = "sha256-bgxVdj1t6Z6P2d63o/I8gaSo751a/J4Xoa2WCVB1tc0=";
}
