# This file is automatically updated by the output from
# nix run github:etu/nixconfig#vscodeGetLatestExtensions Vue volar
#
# Store page: https://marketplace.visualstudio.com/items?itemName=Vue.volar
{ pkgs, ... }:
pkgs.vscode-utils.extensionFromVscodeMarketplace {
  publisher = "Vue";
  name = "volar";
  version = "3.2.6";
  sha256 = "sha256-1R5N3JjJUZ/KPYXGq/VOzbMmQj1fzrK9HrAjA8Ja2a4=";
}
