# https://marketplace.visualstudio.com/items?itemName=github.copilot-chat
#
# $ nix run github:etu/nixconfig#vcodeGetLatestExtensions github copilot-chat
# $ nix run github:etu/nixconfig#vcodeGetLatestExtensions github copilot-chat 1.101 (optional to specify version)
{pkgs, ...}:
pkgs.vscode-utils.extensionFromVscodeMarketplace {
  publisher = "github";
  name = "copilot-chat";
  version = "0.28.1";
  sha256 = "sha256-xOv1JYhE9Q8zRXoZVs/W1U58+SdbJwR5y354LLfKeDQ=";
}
