{ config, lib, pkgs, ... }:

with lib;

{
  # The NixOS release to be compatible with for stateful data such as databases.
  system.nixos.stateVersion = "18.09";

  # Use local nixpkgs checkout
  nix.nixPath = [
    "nixpkgs=/etc/nixos/nixpkgs"
    "nixos-config=/etc/nixos/configuration.nix"
  ];

  # Local overlays
  nixpkgs.overlays = [
    (import ../overlays/local/pkgs/default.nix)
  ];

  imports = [
    ../overlays/local/modules/default.nix
  ];

  # Enable common cli settings for my systems
  my.common-cli.enable = true;

  # Disable root login for ssh
  services.openssh.permitRootLogin = "no";

  # Define a user account.
  my.user.enable = true;
  my.user.extraAuthorizedKeys = [
    "no-agent-forwarding,no-X11-forwarding,permitopen=\"localhost:8001\",command=\"echo 'This account can only be used for weechat relays'\" ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDELIFRmA2C93sjHebScoj8QPynqdYyl6fgYLOrBMsBKQAKrzsfF4wmA/LYo9Z89l3TmpMqzd4C/315HFO6sO7iHVrUfsC0lToA+FOcN7D40pr8m+AaQtVSI14Mlz4GY3fyeyYyssz7XXMn9LEzgZ8SxZh06YLJM9yL1kprBoRXe3Bxbja38JBSl+8xBWRyNrQBPySrTeuoxRYbJ8DUwtOeSElSP6YDjtMut4PbjLXJ2GNHavXhoQaLiZsW4c4YzcMzjiKEmAZWNg2cNuljXMf3KoKCbxqiD9zWidWhKdMuT+XhuDzTt89JAdWWStkj2N++eeESRozHmDBp9PROJx7Z etu@android-2017-04-24"
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDjJcOzWajRgpgZ/uHRNUSpm6giWoXAQL995e5mE0J3aZc/aF68svXjhvpjkRJRFMWJUkwngtQiP+qdFAFDdKI2t9sMeMRuI54HRIQUoeDou8dNPcTlXQjV2DcdOt7GedjlAexdzV/WitNlZC255a8200iQ7e5D9WjSibHpeBrMLhxYGGd/Lzvs5pn6jTJaTFVlCiosh/vnP/rP+W1ruWymVdMxe/6s3ExeIxPeWmpR8zuk30izJ+Gb2jCAzH5lWjpL8BYinVkhbxIyqouaa0Ycgs1/ohcKvRC5Sg/2Or2pJ8rol1aTq1Zb7Z0j+DaXtF3smnpm/gWnuFwin6xiuMjyR20ItI/VOBjZZcLxtDvleXCpBTcZHBaCoGl2YvuZ2GpDSTtF8TPCb3LDQry9LXFUULH650u8bCQvktKGUnuVnwte7wVQGLgTUR+0PsyE+yK34BPWAGxKtowxfMQ59szKo4bY/+WZyll3fySsBeAsGvNPHH9D+Uu3B28Aufyn/1nVQLJKH3UjcVhOeJpnVGw8VBEh8l6gt9wa2VQFP9Wpnlxpbpfr9Q5p6Ssmj4l+fal+PwElN36ltmDs9tfAvoNgxBiG9f0zwYq/pO5PEvmZCTrDqk/mPqEnZrhSp47Qx65ikwwD88ySs5HNtOgZ3NhrYRk4IacWRMt5ESWvJgDMXQ== openpgp:0x14E6F255"
  ];
}
