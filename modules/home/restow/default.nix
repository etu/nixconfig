# restow: update the dotfiles repository in ~/.dotfiles and stow its
# skeleton package into the home directory. Imported by the nix-darwin user
# module on hosts that set etu.user.restow.enable.
{ pkgs, ... }:
{
  # Install into the profile, so it's in PATH.
  home.packages = [
    (pkgs.writeShellApplication {
      name = "restow";
      runtimeInputs = [
        pkgs.git
        pkgs.stow
      ];
      text = ''
        cd "$HOME/.dotfiles"
        git pull
        stow --dotfiles skeleton
      '';
    })
  ];
}
