{pkgs, ...}:
pkgs.vscode-utils.extensionFromVscodeMarketplace {
  # This actually gives me a quite good file selection experience:
  # https://marketplace.visualstudio.com/items?itemName=kimgronqvist.vscode-ido
  publisher = "kimgronqvist";
  name = "vscode-ido";
  version = "0.3.0";
  sha256 = "sha256-Pr3o7FkVu7r0V+PGcs5BRqBh3iXcXOwPRCb2MtoAZJ4=";
}
