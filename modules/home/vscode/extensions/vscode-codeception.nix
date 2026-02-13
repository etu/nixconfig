# This file is automatically updated by the output from
# nix run github:etu/nixconfig#vcodeGetLatestExtensions joelwmale vscode-codeception
#
# Store page: https://marketplace.visualstudio.com/items?itemName=joelwmale.vscode-codeception
{ pkgs, ... }:
pkgs.vscode-utils.extensionFromVscodeMarketplace {
  publisher = "joelwmale";
  name = "vscode-codeception";
  version = "1.2.0";
  sha256 = "sha256-UiYD2BbumMjUP5PpdIsklBuA4UcxVV8WKePXO8p1e4k=";
}
