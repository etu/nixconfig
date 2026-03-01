# This file is automatically updated by the output from
# nix run github:etu/nixconfig#vscodeGetLatestExtensions ms-vscode vscode-speech
#
# Store page: https://marketplace.visualstudio.com/items?itemName=ms-vscode.vscode-speech
{ pkgs, ... }:
pkgs.vscode-utils.extensionFromVscodeMarketplace {
  publisher = "ms-vscode";
  name = "vscode-speech";
  version = "0.16.0";
  sha256 = "sha256-JhZWNlGXljsjmT3/xDi9Z7I4a2vsi/9EkWYbnlteE98=";
}
