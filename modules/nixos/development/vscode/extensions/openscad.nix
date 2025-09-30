# This file is automatically updated by the output from
# nix run github:etu/nixconfig#vcodeGetLatestExtensions Leathong openscad-language-support
#
# Store page: https://marketplace.visualstudio.com/items?itemName=Leathong.openscad-language-support
{pkgs, ...}:
pkgs.vscode-utils.extensionFromVscodeMarketplace {
  publisher = "Leathong";
  name = "openscad-language-support";
  version = "2.0.1";
  sha256 = "sha256-GTvn97POOVmie7mOD/Q3ivEHXmqb+hvgiic9pTWYS0s=";
}
