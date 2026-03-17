# This file is automatically updated by the output from
# nix run github:etu/nixconfig#vscodeGetLatestExtensions fastly vscode-fastly-vcl
#
# Store page: https://marketplace.visualstudio.com/items?itemName=fastly.vscode-fastly-vcl
{ pkgs, ... }:
pkgs.vscode-utils.extensionFromVscodeMarketplace {
  publisher = "fastly";
  name = "vscode-fastly-vcl";
  version = "2.0.8";
  sha256 = "sha256-BQX0fX2s/3OxOc+8GWeLGoJjoXvvx7f7r9enCJg71xE=";
}
