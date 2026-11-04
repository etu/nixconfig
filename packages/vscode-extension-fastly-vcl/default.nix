# This file is automatically updated by the output from
# nix run github:etu/nixconfig#vscodeGetLatestExtensions fastly vscode-fastly-vcl
#
# Store page: https://marketplace.visualstudio.com/items?itemName=fastly.vscode-fastly-vcl
{ pkgs, ... }:
pkgs.vscode-utils.extensionFromVscodeMarketplace {
  publisher = "fastly";
  name = "vscode-fastly-vcl";
  version = "4.0.1";
  sha256 = "sha256-4xAXk3tiw5qJ5IfhJlD8lCrLRtWlNyTbju3lixVBhOI=";
}
