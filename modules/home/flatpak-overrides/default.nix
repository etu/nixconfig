{ config, osConfig, ... }:
{
  home.file.".local/share/flatpak/overrides/com.slack.Slack".text = ''
    [Context]
    filesystems=~/.XCompose:ro
  '';
  home.file.".local/share/flatpak/overrides/com.discordapp.Discord".text = ''
    [Context]
    filesystems=~/.XCompose:ro
  '';
  home.file.".local/share/flatpak/overrides/dev.vencord.Vesktop".text = ''
    [Context]
    filesystems=~/.XCompose:ro
  '';
  home.file.".local/share/flatpak/overrides/net.lutris.Lutris".text = ''
    [Context]
    filesystems=/data/local/home/${osConfig.etu.user.username}/Games:rw
  '';
}
