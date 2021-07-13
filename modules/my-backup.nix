{ config, lib, ... }:

let
  cfg = config.my.backup;

in
{
  options.my.backup.enable = lib.mkEnableOption "Enables backup related thingys.";
  options.my.backup.enableSanoid = lib.mkEnableOption "Enables snapshot creation.";
  options.my.backup.enableSyncoid = lib.mkEnableOption "Enables snapshot syncing.";

  config = lib.mkIf cfg.enable {
    # Enable sanoid snapshoting with rules for creating snapshots.
    services.sanoid = {
      enable = cfg.enableSanoid;
      interval = "*-*-* *:00,15,30,45:00";

      templates.default = {
        autosnap = true;
        autoprune = true;
        frequently = 7;
        hourly = 36;
        daily = 14;
        weekly = 4;
        monthly = 2;
      };
    };

    # Enable syncoid for syncing snapshots.
    services.syncoid = {
      enable = cfg.enableSyncoid;
      interval = "*-*-* *:15:00";
      commonArgs = [ "--no-sync-snap" ];
      sshKey = "/persistent/home/syncoid/.ssh/id_ed25519";
    };

    # Add known hosts so syncoid don't bail out on unknown hosts
    environment.etc."ssh/ssh_known_hosts".text = ''
      # Home file server
      home.elis.nu ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJRZYWjxAqloB5MZtxBHkckZhKi+3M1OObzBdyi7La98
      home.elis.nu ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDXPQN5sjBXNKPMT7SzGIywlS5zupc2p/6uYiUxSzDzN39hhZDt5iKVbKTvmtSvHU3b/bOLA1U3DKvVZPbSyuaddmN7Oo0dh4EaN8dkpSkH5xuWb7bwrrZ9ZI4Ggp0NgnYBcPp5xWs8jrJbhnFQDS/Ev71UAXeRRhcNu6QtHKJrcLPh+FTw4HkoWDD7E9n0kFovBzpkm9bm9tRiuuUzKu8d1ue+h3e3L7Wku6jhsM4xDKflv1I8O3HH6zgA+cdy3SJ1QDMBizTHzkW7tuWTjayspSrNAYnm3Yn4u5KBIPLtB1F6xe+bC8FAp59ZzflGHzwtz2psj4OlnWjmiqUde+cxsk4012neNf2UN2+q5w8mGN9BzETfxmwykCrKxc5nWk7yeaNx3A3SIJvT6hI+5tC+PZdY4FGFQm/StDeYXumFn0bjy+vW/1uLkF1AmsxbDmUO/Dj4cbx7Dh3m+5mCGQ6UyShD56lxKsdZBUTnawaK7WZhSiY72KS7EU9Li4Uk/hqJG/qcS+zmSo32ZpbwtIEOtRNm8ebTHjpvpv1vMY6n76R/xLJEWKwvE0K6EdGjD5RzrYWSIUVnsk+nf2/BNN6pGDesZhL6eltfKEyfJvBVwm3cPRLaE/kxehmcPFmLH4i1bijbDn4JpH7PWIQMyX+UF4+SpEhXtQ2UxdfUllZSow==

      # vps04
      vps04.elis.nu ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIC/UewM3gYrEFZdaD6zDdP9Vkq1W9gOIXJoBG/ram+Fo
      vps04.elis.nu ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDUf9GAcV8GIaIv7WyIok2qM12VcfpbDVpLr421YJsNkosrEkxmLau6RreP05O0QRHwIaI9XQNYOQ5Mt+XDIWg/SrprH5QZRmemxcHKzzcHf0PQXDcDXOZO7jqEv9B6bk36/heej/p3Yj8z2jQzlVrym5vuO37ShGoMRMDxeecrfzXCL5tT610JOxNMBoPy+ibSh4rWGONeCNfoH3V+Sfp7JGhhmVSyHHdVdRafQzxlrNWZVmhrXfxhxK7wyACjSXSQKGyYCZTWOtxAe3OAd6xqIBMTbYBXA9TsAf/3E7xusSJuvXZv4exym/9rB8FhkpEv3sUSx2zwQ9/ZUZm8bRgnn68U0VCDSYG/PzvYBZ0DaUDPa+Ueqn5gYh4O3eNEqk+1lLTi4iaGsLdDVMmqHBDj6UY2kmgkn8MiU4KutD5U3/vXPEuuo4IklKl5ELxYmvHhn4ZWKvluzCutc8AAAoMS1oga1fYJQxeAY1l8HUVprDtoly/PWAP+OjVuU82BsC3573OJd2bvHWd0TUffu40m93eO0aOhknloL6rL+we61TXqZn18OwU7W2QLpsKJk38k5OB5RYljQ53FqgxznJZ3uRJA/g5gt7TP2Rm4X/2y49a5PN+kvUe8FuPiC66UFakPWU/l1D9uKvnZZl6fB1fndRXFQkq2LKHmaRBwRzdFjQ==

      # vps05
      vps05.elis.nu ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJZpIEdJ81F205GHvDa//O7l4SXNUNMkiN2wcqY/baDT
      vps05.elis.nu ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDUVCtUy13WsWEzH2Ce/du+B8tTBqTgiLZrkJ6+jgKdkJBS2L9B9B3nQRHppi/iKj+48Gfm84fawtx0ZMvxnXbUlucJ9n9mCxTCrUazTBNz7Melk4tZ2ry0pb/bXoyDMxm8V5MiGIBovJkjDhUiho6YICYSh/gj36SpVOA2IX0nhClzfOMkvudiN9DWmSzhjiHa2tLUJ/WYwn5DaOlLvZ1h7zzZBx6txrWAoyVXuAgRsfq+FaC6pAiEkt6RJ/IkqSCoYuHqZm8ESc6prbK7lWr0ocpfuEh1vM6ooc7LHzfUVGWEZGQ+j3i231Svam9fr/Iiq3YGqnumqG30/EFDR4ZzrzA7U4tJDOjSsgaDB9v0fbfdDtGwfxaafbwgmnlEjHFKiMfxoAI1q3HFI7JJZyEFeWgFlCqyQ4ZVXJ52HgZdVj6tEfLD6rxS1jWCGhodiBYPWcyQAgJojj+ljZPlGOFBnexBe5HF6TTYw8+6yHFPYLmW19N9Wvc7O1FF3jzaIkvpHTf/WJ5ORkW4bKxxkUbtMbaf6QvOjYNNOnV6kKj0JjwH3XOQr5Po3RlHmSTZ/VW/ndddSIrroxDAmo3t3RWpQYSfRRZ0h3yM49gCdx4DtHCkH+2eJEoVhH9OAnjRDQlO+n8kfp0BcVL3K72y1KPHz0phGd7SMoZ2MWbRx9KOJw==

      # Home kodi machine
      192.168.0.105 ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDrccd4BZHoVOFr37koZoimtAacLVtS3GzgFYdC1ptPA
      192.168.0.105 ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQCeXyo7pOAC1v3//GNQhd1g7DiHMhGZh7fbzIDcR10HKx50p+hAuj1Hs90YzfWpMlYUWLOnowoIu+ua5280bFYaGkg68yvFCt/5582AxKQbYwGMIRU1eoj9e60/a1M2pH/N558LGgRECLOwZVvIgXZOfr5Z7GM2IaNS+1/Qa0IIvHdM1tvy4K7WJXpSg98+jZ6B+zF7jvq4R90S2HM8tMFnTtVGQwDpqxr9ZShn6A77MWiazyLNcwypbyMl9Y8L0qB2osrKditR5qVLkDhv+2QeHqIwUpyXUBFjq1ZpnSpfeIndLdk1WVmrP8dQ3M7nvKVG3d6LGrH9u9x94byOxo44MS547LiGh8dBTNkv1LGbnVNKQZNI4LcGqnZ9rna2VmBsVN7jW4Ig0owXeyyNiNeL/nwKd42ohXvuw7Qx13fvvinDZNMx2BhvUEl/8pFMV1ZPj0xJRw3HlqT4J0Y2OwksUhDN6L8d1xZ20gUGhJqfhCXBSyPQNHP5CZIjMZ+//OA66uXbC6R3UWv1gn9KWuMO6hzpUkmzYcT9JosFTUxT9bPlOOqMs4TqQubw9/2dW6yABRkuWMJ5v7YuLRwMA88aLGrr6WYTRoPCzx1RS3JWrFKmfUNaUADp3Xc/uXNQy7mWWSjZ8BPnFGZ7SjQhxMow5E0btm0NQnoFQGs67Halnw==
    '';
  };
}
