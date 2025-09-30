# Wrapper package to expose the live ISO as a package to make it
# easier to build build with nix build .#iso
{ inputs, ... }:
inputs.self.nixosConfigurations.live-iso.config.system.build.isoImage
// {
  meta.platforms = [ "x86_64-linux" ];
}
