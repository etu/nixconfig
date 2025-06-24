{pkgs, ...}:
pkgs.vscode-utils.extensionFromVscodeMarketplace {
  # https://marketplace.visualstudio.com/items?itemName=Leathong.openscad-language-support
  publisher = "Leathong";
  name = "openscad-language-support";
  version = "1.2.5";
  sha256 = "sha256-/CLxBXXdUfYlT0RaGox1epHnyAUlDihX1LfT5wGd2J8=";
}
