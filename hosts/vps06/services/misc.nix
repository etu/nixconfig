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

  services.nginx.virtualHosts = {
    "ip.failar.nu" = {
      addSSL = true;
      enableACME = true;
      locations."/".proxyPass = "http://127.0.0.1:8123/";
      locations."/".extraConfig = "proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;";
    };

    # Served through the Cloudflare tunnel, see ./cloudflared.nix. The
    # service answers with the first X-Forwarded-For address, so set it to
    # the visitor's address (restored from CF-Connecting-IP) instead of
    # appending to whatever the client sent.
    "ip.elis.nu" = {
      locations."/".proxyPass = "http://127.0.0.1:8123/";
      locations."/".extraConfig = "proxy_set_header X-Forwarded-For $remote_addr;";
    };
  };

}
