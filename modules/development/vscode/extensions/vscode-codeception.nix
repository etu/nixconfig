{pkgs, ...}:
pkgs.vscode-utils.extensionFromVscodeMarketplace {
  # https://marketplace.visualstudio.com/items?itemName=joelwmale.vscode-codeception
  publisher = "joelwmale";
  name = "vscode-codeception";
  version = "1.2.0";
  sha256 = "sha256-UiYD2BbumMjUP5PpdIsklBuA4UcxVV8WKePXO8p1e4k=";
}
