# This file is automatically updated by the output from
# nix run github:etu/nixconfig#vcodeGetLatestExtensions wongjn php-sniffer
#
# Store page: https://marketplace.visualstudio.com/items?itemName=wongjn.php-sniffer
{ pkgs, ... }:
pkgs.vscode-utils.extensionFromVscodeMarketplace {
  publisher = "wongjn";
  name = "php-sniffer";
  version = "1.3.0";
  sha256 = "sha256-dPF1CRX9WVQFyC7RZxiPDtIg6+oUituY0qEn5Hipd5Q=";
}
