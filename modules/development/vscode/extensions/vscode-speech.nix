{pkgs, ...}:
pkgs.vscode-utils.extensionFromVscodeMarketplace {
  # This is a speech to text extension for copilot:
  # https://marketplace.visualstudio.com/items?itemName=ms-vscode.vscode-speech
  publisher = "ms-vscode";
  name = "vscode-speech";
  version = "0.9.2024032812";
  sha256 = "sha256-VC9IrA+gwMBOE260KGBnFsfYxgMJfOIoDVsJKUW6uqI=";
}
