# This file is automatically updated by the output from
# nix run github:etu/nixconfig#vcodeGetLatestExtensions github copilot-chat
#
# Store page: https://marketplace.visualstudio.com/items?itemName=github.copilot-chat
{pkgs, ...}:
pkgs.vscode-utils.extensionFromVscodeMarketplace {
  publisher = "github";
  name = "copilot-chat";
  version = "0.30.1";
  sha256 = "sha256-itANvwMSzFBPnU4B6erEXO/x3SNlqHygXlTE6jLc+0U=";
}
