{pkgs, ...}:
pkgs.vscode-utils.extensionFromVscodeMarketplace {
  # https://marketplace.visualstudio.com/items?itemName=wongjn.php-sniffer
  publisher = "wongjn";
  name = "php-sniffer";
  version = "1.3.0";
  sha256 = "sha256-dPF1CRX9WVQFyC7RZxiPDtIg6+oUituY0qEn5Hipd5Q=";
}
