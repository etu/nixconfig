# This file is automatically updated by the output from
# nix run github:etu/nixconfig#vscodeGetLatestExtensions TheQtCompany qt-qml
#
# Store page: https://marketplace.visualstudio.com/items?itemName=TheQtCompany.qt-qml
{ pkgs, ... }:
pkgs.vscode-utils.extensionFromVscodeMarketplace {
  publisher = "TheQtCompany";
  name = "qt-qml";
  version = "1.16.0";
  sha256 = "sha256-QWjUSbBtHIdxYZyRBDn64HXhvSwhgzm7DjDaed6lNds=";
}
