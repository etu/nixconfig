{
  flake,
  ...
}:
{
  imports = [
    # Import the ip-failar-nu module
    flake.inputs.ip-failar-nu.nixosModules.x86_64-linux.default
  ];

  # Enable the ip-failar-nu service
  services.ip-failar-nu.enable = true;

  # Served through the Cloudflare tunnel, see ./cloudflared.nix. The
  # service answers with the first X-Forwarded-For address, so set it to the
  # visitor's address (restored from CF-Connecting-IP) instead of appending
  # to whatever the client sent.
  #
  # ip.failar.nu is a Cloudflare redirect rule to ip.elis.nu and never
  # reaches this host.
  services.nginx.virtualHosts."ip.elis.nu" = {
    locations."/".proxyPass = "http://127.0.0.1:8123/";
    locations."/".extraConfig = "proxy_set_header X-Forwarded-For $remote_addr;";
  };
}
