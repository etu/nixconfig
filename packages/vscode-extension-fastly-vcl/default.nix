# This file is automatically updated by the output from
# nix run github:etu/nixconfig#vscodeGetLatestExtensions fastly vscode-fastly-vcl
#
# Store page: https://marketplace.visualstudio.com/items?itemName=fastly.vscode-fastly-vcl
{ pkgs, ... }:
pkgs.vscode-utils.extensionFromVscodeMarketplace {
  publisher = "fastly";
  name = "vscode-fastly-vcl";
  version = "3.0.0";
  sha256 = "sha256-ybI+Tu4XTc5xObFKfcmZ5ihRp3y2OWY2C/lGbUY9oBk=";
}
