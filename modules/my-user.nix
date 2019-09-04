{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.my.user;
  uid = cfg.uid;
  username = cfg.username;
  extraGroups = cfg.extraGroups;
  extraAuthorizedKeys = cfg.extraAuthorizedKeys;

in {
  options.my.user = {
    enable = mkEnableOption "Enables my user.";
    uid = mkOption {
      type = types.nullOr types.int;
      default = 1000;
    };
    username = mkOption {
      type = types.string;
      default = "etu";
      description = "My username for this system.";
    };
    extraGroups = mkOption {
      type = types.listOf types.str;
      default = [];
    };
    extraAuthorizedKeys = mkOption {
      type = types.listOf types.str;
      default = [];
      description = "Additional authorized keys";
    };
  };

  config = mkIf cfg.enable {
    # Define my user account
    users.extraUsers.${username} = {
      isNormalUser = true;
      uid = uid;
      description = "Elis Hirwing,,,,";
      extraGroups = [ "wheel" ] ++ extraGroups;
      shell = pkgs.fish;
      openssh.authorizedKeys.keys = [
        # Old fileserver
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILPvVYtcFHwuW/QW5Sqyuno7KrsVq9q9HUOBoaoIlIwu etu@hactar-2016-09-24"
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQC0KS9IW6JXQpKKi3Igtm5e/s0Fe2iGrp3dVgpWS4k/3NRKruOonMw+akfwrlPU+gcIPPvUZQfs87doBa8s767N6NMnErfCy6aJgo1RMV6k1rTzhyV4Cv4imVGKhLv84B09v8Jqp4fxqDJ2utuh/PmJdwpSKcjckayfVADZBzG1cooUcCKXu8h20R8Gr+7Zvsh53okjR2P48HSj1I3gquGgD9vwaE0jkPq5Ayi0jviz2rXiuRPIheN/B1PPh9FyB5SGIiOEdkboGdOFOfZwgrXntqKqOjVeiOwj4g3/bE9POMy57uIY6lYw4cP3yTotEBVZpjvDWEj/ko84NYOXOdvehQJInmjQww6nFxazTzBnsT9Hns7nzn5lYm89RSjUz6Vi6048+b1kpKomWwD0y38gt20jLfo+mC04cvekImMY3hnC6sRK724Z0EXCoz9IeuyeRRUdehQUM9EnErn+MsowWFIx4+r+I5TdLvL2g/hH1MPpngyRSGGW9IWq2eXTGpVkBT1c5du7LutwYSx4X8mqJ5bhMN8udIj9LJjGrpt/hFoLRF2ihgd6GKuq2KJjvIZyzfj8jarBXpx49DJg5aUZ3h2hAtsIB3iqpmU7rpK0HQri6+FJ8+9BCvYBd6YZ2IZPOLHjbo0avqrL0zzSka3fx4oV64WkHkgufxx55QHqTw== etu@hactar-4096-2019-08-04"

        # New private laptop
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJ02fsmCbs2HSQxQV66cA3OnLqbhrZlA+wREUYn/3HzR etu@agrajag-2019-09-04"
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQC22txG69NIxuIrfyn13jAWmJW3ewBFOFRubUAjQWwZGVRJILaLxS02gTeMq3BYxcSthnv8fCkBUwIvPgM+zFh6H5BFAupyQ3jIkXnYWj3BTDUgkLXVdvn5h4d3EpZmevH33FGA1+AEiM9auOO11ueG4n+CrJQtJ71Ehu8yopouP4vZLhTyJVEbDom7AYwhF0muemLRVeRp/AEKoJZeGT5gUZCSB/N1ndEbXHOI++XODuf6DUMFM+jPQKWPnMeS5PUCtlsTInOy+TvWXRFKL6pB+DAT0Xce9azRH7y3UKE0YocdPwJ4XMIbuuFFt/f07bEVdJr1e2wV23jAiUNiWGCtsr5f4VqCUJFHKAsxGiuXiUywjC7578mVZWkw1mEGo5NuqOhwmTkvJarQxbfeAOs4ryfqjLQPW7Pddl/O3xBLrmsDA4IqfewHEpB2w78DyC/NdP95sTWtrOb3EeEMyzlCuKAMUiRhcZKv8WeLFdl1MEn10P1ls3uH5bIUAYdAyVpeFu5iZXLHHs5H1nJdGLARWl7WHvfF2X80XnNvgUt3XA//NTI5SZqunD90GHwyZKqqoao6AyaTRZlWGA95E/Kd92EBG5TvWq0AOjnXYIxdMY6oqC6aiQMxpH9IVHlg4nUdPEQEMYpzpNMSmb/H1FejdKcpH83PlGHYPZ0tz0jE/w== etu@agrajag-2019-09-04"

        # Old private laptop
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIEWP6z+bCjt2XRO+mFraSRx4lrwVCVysYzruC14aQmD etu@ford-x250-2019-04-19"
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDAklhbUec4mVYx69Y4vRrHAR/hsAH+HAjrGtoG3mtQYrbAlOebenpZ6HfLv0FnyEnIepRkmsXpe+W9fcaebFVPP3tnhYoiFhibAQzfbV4yAkZtxzNUIeBBtmtvn9Q0yQm7dLDgh+ZioF8rpNefa6klOmk+lBOXKSRw+aqrSOUYr1VYnACd9Drewvjk7zh4xvqtoeBfcGHmMstiKoDCJf35T1jI0rXgJq1BydXi64zB3jInx6MJ60PmSSaglgznPb2SLRkf3WSxmYcyrxh4DRWN2+c/OjzL4k15ewhRnyI7ZAazXIj/+j/8ZTG5KfWv1JwAf/Lccd1iVgek6A/00J7oyFFcRuQo4kd79FtwpDWMcoBx0ciIXDwuFQDM2z+rsjOEKuP2yfDgEtqJX62qYrtAbazlubZxbkI57VbijEQlz9W9RAGRya9XJm8PAfiseuHRwRm2ptH+hZhSUU6xgLpPPYb7Vqnux0zQ4YRU273m7w4KKpfvB23q+hgtm57kOq/l4A5sG7Ni32YT7MQAsvvibdFp1DdnpMTAfLbS9mltcWterLitVMmNjLSBETcK/Q/3HKBxp6p2S7cJF/DAMPf8iaW3ofRigb3m+SXawgiSUaS2b420q31a12eKj4DprzA3Ycn95FJO7UsmlCIRhE+5qnUVPBu9/9MD+og9zjoQww== etu@ford-x250-rsa-2019-08-03"

        # Work laptop
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQC30zMfq5/ZBnLPXiz4qtTsg3SU6voKQMumADNhTpVSKo3erANR5zTb1WPfjM4IWLCcWfksWDNOOeMaKM0hGgdnGbfrXpIOJwKNHaSp11cvQ6wTMAGV3B3ItJHOV+Czw4kEeUB+Tic8m+U2jnTPLXC4x3B7bdXHhdhmQbTpEq9pabe8eRQM54/9SuG6M9y8G3g35s3edsXrEnh/OI62a66F5aOugQH4fX5ehfGg3zk7LLu7U8bX9FGOeOVCCEBsfm5ysczNAO3v1iA4G9N8vgfAHJNZfKglYSQIi9nyURxcqT511OFTGK1cyWHGjqCNK286Plx90u0SVQvvG+9hkq2l/kbgmNpEYlTmAs22y+6j7R+gpSCMxJSfjXfJeyVcCTKo2CT9+SQRDz+pz+wyv/NgnaqXuP65RlwS0OIhdT6YheaFfbhkuMzFD78VDWOacLamWVQz/yTe5o+GhTavWVZyZ4Y9Wf+LB4sQtM9S5AqWpSCHDfA9nF6E9oWPbAZ4l+VD4vNQdrmByh+3uk+XP9/ytJVyJDd88MmxnC1yiN3xT7rQaUoUCmYy0Z7BLBYvBb2fU+JrJ8Eew6uf23SGYUtZuxanNANslBdvD0t68xCEHIxsVecpqRtbG8699bZEoWWbAFS1WO5EBAcYRfKV/4SQxLTO0J+HGQSRkgL7Ex1ioQ== etu@phouchg-4096-2016-04-18"
      ] ++ extraAuthorizedKeys;
    };

    # List packages installed in system profile. To search by name, run:
    # $ nix-env -qaP | grep wget
    environment.systemPackages = with pkgs; [
      ag
      direnv
      dnsutils
      jq
      nfs-utils
      sshfs-fuse
      stow
      testssl
      youtube-dl

      # PHP utils
      php
      phpPackages.composer
      phpPackages.xdebug
      phpPackages.phpcbf
      phpPackages.phpcs
    ];
  };
}
