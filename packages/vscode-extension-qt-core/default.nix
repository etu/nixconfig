# This file is automatically updated by the output from
# nix run github:etu/nixconfig#vscodeGetLatestExtensions TheQtCompany qt-core
#
# Store page: https://marketplace.visualstudio.com/items?itemName=TheQtCompany.qt-core
{ pkgs, ... }:
pkgs.vscode-utils.extensionFromVscodeMarketplace {
  publisher = "TheQtCompany";
  name = "qt-core";
  version = "1.16.0";
  sha256 = "sha256-6NBWsK7ljx7Ie3xvOrjBClyvwQP1UltCna1jQHJST5U=";
}
