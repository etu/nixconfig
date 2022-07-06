{ config, lib, pkgs, ... }:
let
  # Import age secrets paths and metadata.
  ageModules = (import ../../../data.nix).ageModules;

  # This script originates from:
  # https://github.com/webosbrew/dev-utils/blob/main/scripts/devmode-reset.sh
  #
  # It's however modified to work with NixOS client configurations and
  # the fact that I deploy the secrets using Nix rather than having
  # them stored just on the device.
  script = pkgs.writeScriptBin "webos-devmode-keepalive" ''
    #!${pkgs.runtimeShell}

    # SSH to the TV to get the current development session token
    sessionToken=$(${pkgs.openssh}/bin/ssh -i ${config.age.secrets.webos-devmode-keepalive.path} -o ConnectTimeout=3 -p 9922 prisoner@192.168.1.174 -o HostKeyAlgorithms=+ssh-rsa -o PubkeyAcceptedKeyTypes=+ssh-rsa cat /var/luna/preferences/devmode_enabled)

    # If we didn't get a token, read it from the fallback file
    if [ -z "$sessionToken" ]; then
      sessionToken=$(cat ~/.webos-devmode-token.txt)
    else # otherwise, write the fallback file for the next time
      echo "$sessionToken" > ~/.webos-devmode-token.txt
    fi

    if [ -z "$sessionToken" ]; then
      echo "Unable to get token" >&2
      exit 1
    fi

    sessionStatus=$(${pkgs.curl}/bin/curl --max-time 3 -s "https://developer.lge.com/secure/ResetDevModeSession.dev?sessionToken=$sessionToken")
    echo "$sessionStatus"
  '';

in
{
  options.etu.services.webos-devmode-keepalive.enable = lib.mkEnableOption "Enable service webos-devmode-keepalive timer";

  config = lib.mkIf config.etu.services.webos-devmode-keepalive.enable {
    programs.ssh.knownHosts.webos-devmode-keepalive = {
      extraHostNames = [ "192.168.1.174" ];
      publicKey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCgTzU9EWtFgBiFJ39Ql+LOVOPZnnDAicmHEZ+X1lqmEfWGvhFnTQ0wJTq9rFhVzeOYsabGW2znMSaeD4aPbR6fhb9TMFj+R7p5bNjeDadxVF/We1DiFgIDxyZmKHhfNFf9+rIplwuywIz5kYaXZzjcYNcvkjXLlont6EOBmSFNlZBUitqBssgqu9YLu5xAzU0F3aSGSGFgqCz8GRVa7haVhG9OwZXgrbaDBAfMkhdlcz1CyaJMeQufdi+qY1d97is14IM9wqFoehc5gU2NQhhjkivht68lk8s0Y3KhNGzFa9IYRb4CBc+8AdOdiZCIFjrbMLaCaea74B7K3fi1b2uB";
    };

    systemd.services.webos-devmode-keepalive = {
      description = "Service to renew and keep the webos devmode session active";
      after = [ "network-online.target" ];
      wantedBy = [ "multi-user.target" ];
      startAt = "*-*-* 14,17,20,23:00:00";
      preStart = ''
        echo "Checking for nameservers to appear in /etc/resolv.conf";
        while ! grep nameserver /etc/resolv.conf > /dev/null; do
          echo "Waiting for nameservers to appear in /etc/resolv.conf";
          sleep 1;
        done
      '';
      serviceConfig = {
        Type = "oneshot";
        User = config.etu.user.username;
        Group = "users";
        ExecStart = "${script}/bin/webos-devmode-keepalive";
      };
    };

    # Include secret ssh key
    age.secrets = {
      inherit (ageModules) webos-devmode-keepalive;
    };
  };
}
