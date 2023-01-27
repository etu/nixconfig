{
  # Age module definitions that is to be used by hosts to get secrets.
  ageModules = {
    cloudflare-api-env.file = ./secrets/server-main-elis/cloudflare-api-env.age;
    hashed-etu-password.file = ./secrets/any/hashed-etu-password-file.age;
    hashed-root-password.file = ./secrets/any/hashed-root-password-file.age;
    hashed-ozeloten-password.file = ./secrets/vps04/hashed-ozeloten-password-file.age;
    "etu@aarch64.nixos.community" = {
      file = ./secrets/laptop-private-elis/etu_at_aarch64.nixos.community.age;
      path = "/data/home/etu/.ssh/etu@aarch64.nixos.community";
      owner = "etu";
    };
    "etu@aarch64.nixos.community.pub" = {
      file = ./secrets/laptop-private-elis/etu_at_aarch64.nixos.community.pub.age;
      path = "/data/home/etu/.ssh/etu@aarch64.nixos.community.pub";
      owner = "etu";
    };
    server-main-elis-initrd-sshd-ec = {
      file = ./secrets/server-main-elis/initrd-sshd-ec.age;
      # This is weird, if I change this path to be in /data, it
      # doesn't get read on boot or when needed because... somewhow
      # it's hardcoded to it's old location.
      path = "/persistent/etc/initrd-ssh/ssh_host_ed_25519_key";
      symlink = false;
    };
    freshrss-password-etu = {
      file = ./secrets/server-main-elis/etu-freshrss-password.age;
      owner = "freshrss";
    };
    flummbot-toml = {
      file = ./secrets/vps04/flummbot.toml.age;
      owner = "bots";
    };
    grafana-admin-password = {
      file = ./secrets/server-main-elis/grafana-admin-password.age;
      owner = "grafana";
    };
    syncoid-server-main-elis-ssh-ec = {
      file = ./secrets/server-main-elis/syncoid-ssh-ec.age;
      path = "/var/lib/syncoid/.ssh/id_ed25519";
      owner = "syncoid";
      symlink = false;
    };
    syncoid-workstations-ssh-ec = {
      file = ./secrets/workstations/syncoid-ssh-ec.age;
      path = "/var/lib/syncoid/.ssh/id_ed25519";
      owner = "syncoid";
      symlink = false;
    };
  };

  # Public keys for a bunch of devices, users and hosts.
  pubkeys = let
    etu = let
      # Android relay client
      weechat = [
        "no-agent-forwarding,no-X11-forwarding,permitopen=\"127.0.0.1:8001\",command=\"echo 'This account can only be used for weechat relays'\" ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQC5veqjY/i/5k6QTyluIBSsYGwgKxUsdsVWgcLbvzWb/GnTx1Ia1SXYvExGrdbfEGyeZEdbsK+PAJlMVJegt/jgE8015Ga0jRwlW6y4G19FB0UrCJU/vVCnyfuH7DgswtVQ98795VET4tfWVZTziaTyrPFi+lAw7afsI5orL5FzzH6+k33M3KSDuSoIgdGgcl+KjVyil4EbO+1k0TqtE7hnD3Lu23G2gyHOSSSB9B6cFrYWWVdxNttUCkNWqwpO6IDZ6ZzBgLZSxxDqgLBjjDYlbtGw4dWk5+39x72K1Rjsj+IiCxgwpk6lCg/QFe0QGIQN9QAe07Frm5hYPx4hXltamePbWti/K7nq4HEsG389R4RjWc6CXjY9SXt0kWLlrdN+cdCncgvQxlKlm/d15BNpnXE7P2aRyGmCeyQBuMCaJ1L5EShC/QXudl/b348dZN3z+51BYVLOQb5J7p2dWUTndN2W4lKf5k2vNeM2vtiZKSMy755GIbwEGv2PX2KYsBH8jcCHEW9sw4IMZxNQW/85dsjW6FPu6EyTlMAJC7H9BO1PJGb7hCTpLic5U3Y/q0Yrj/z/r04z5OX+8BnR+qug56fYMzGJ7oCYAG+JwP80Uw0wIThxiZX0eB7dhuo4DrIlMo30WJXMNw5d5z/WrYYLkifMIirZ/Dal/gfu7QuF9Q== etu@android-2020-08-14"
      ];

      # New fileserver
      server-main-elis = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDvsU9DbT9Buk0FcEA6cuq8UdE1wG+bD0UpyGoxJc93x etu@fenchurch-2019-12-22"
      ];

      # New private laptop T495
      laptop-private-elis = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKALrQoSasNAaAvERCMsztZkezg0gRSFXWbc1vXpA1+C etu@laptop-private-elis-2023-01-27"
      ];

      # Work laptop
      laptop-work-elis = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINwl7wWkYdxmUutxr2vzPPm7hiM5TuIwhV+YoCjrY0Qn etu@laptop-work-elis-2023-01-27"
      ];

      # Public keys used for syncoid.
      syncoid = {
        server-main-elis = [ "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMGc+oDfq+OCsApi1qsMDx1wlDwfu7oIHOeV0laVdq6W syncoid@fenchurch-2020-07-11" ];
        workstations = [ "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICdtbbrBHT1i29nFdaf54zsJ4Yrt2tOLnNotaRIneazp syncoid@workstations-2021-07-11" ];
      };
    in {
      # Include all separate units
      inherit weechat server-main-elis syncoid laptop-private-elis laptop-work-elis;

      # Include a meta name of all computers
      computers = server-main-elis ++ laptop-private-elis ++ laptop-work-elis;
    };

    concate = [
      # Android relay client
      "no-agent-forwarding,no-X11-forwarding,permitopen=\"127.0.0.1:8002\",command=\"echo 'This account can only be used for weechat relays'\" ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQCtXdjEIcVCFwR5AvWxeyxd0Uxv2HYTUOI1+IAsSY5SpLbljA1fk57svmyr2wv3uTjbLoOky4DHvYEnsin1er15K4PTqZjZjd4m3yNOKjtlmp4FJvbXou13ZrRZGeDpFFmBC6ZS6DHUKR6rEbAoyHLcWM2Af8seSFFcmeHGNZukCbot2M3C+DXwwJu7BaFWoFF5Yb9RQMdX8dRCLOx9C8UzHto0WPsVl5bTex0EPv+KACe3dcxoHwNCFgCWXjTCgKBIKbo9ZIELjLwYwdsTVVuUlkH9VLTkTVkzqcUcFQ3HAW31SdwWOK12AwQ2pTsxq/oOZt47Z5CwdFfrRfTFQVWR8akse93zJHRfeie//hnA0fgO6XU+47UY8vXzqHDvWqwypOpwJaohtDDU5kv4j8TMJV9wrEGCULIm7V36WB2DEkYi93zy8k/DBpU5LgP0GSvXgSfNgGjds6Yb22wro31CTIQz8sQZGshCJu3uwtVYIXdXXYA+iStvTBSML9nGFNiUfehMYJdOPo91piQLmn4voiLgG7Uk0sCzoUzXyPMdfOwh0hRnwrmaAb1NI2b1RHx5pyEXBc07SBuhhdqIDTflgxFQGxbIk1T41JgpfuvWTpINhhb4KzeDWJTO5kz0WDr0/N1x7SLimrI7Le2Br8tf5oJbVLaTI0bzuLfGo8sj3w== concate@android-2020-08-15"

      # Computers
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDJR9Mm7KTTxZ1A9xIqv8RyGB5Yp08RE3Ns0M4fsyTLjdIw08RjD+Up7PA1KKgoGsTTzk9670lrQVohpUE96MeoteZnZjFUEGv1Oiuy4yVs/jy5ngwtdchh6MU+xOvnhZ/7m9inoRFJXG1OTEsipKVrCXctjNNPY7XIM99QH7wH2Ebua8H5QFnvclTIpN2d/QhhHLlffye++ww5lltU4AkSt72QlAqUOefsVdQDS3TuXxu1+OgQFnK8eaz517pktsLkDEY/9lkqSNP2okTTmaj928TEZHYDYt+IkoMZxoGO8iCFtKsJY24gSdl3qysVPVWwPpS1TxV/sud5iPQzBXfHRtbNEnQ6ewImLsvkFmGYhIQ3gJmwHbRSUtg7TJ5TZOEez8Wc6cFOwqE7ltlM8ob4etPcmo3qiJH6ecchOwwrY4HWPdw+uYBAvFVaNEEf/elWjoZTC6BSb7Bf8sba8m/98+QYj0LWL0FDDarlx6ZUnkbROk44WlmI6nk+MVKvb/rjK/sbATxruj9ARWhQUXgA6PFytmEj5OhSK5ibcRDB23cTMk+DK5jldDLLPP7gi6NlhhdfoBJEtgXNJqCDrpgf8yLG7bXJYWroMArRFJXcQc4Ogxd10K5Ec6TDPvwDqrMoFEMW1X20jZkdF+4wqcQUs2CD96iHao+QxxYoScgV7w== caroline@neji-200521 "
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBSb9gF46pEMA0hZzR6/EFbXzsV10RtB2dcm3HFCz0Ob concate@rocklee"
    ];

    talyz = [
      # Android relay client
      "no-agent-forwarding,no-X11-forwarding,permitopen=\"localhost:8003\",command=\"echo 'This account can only be used for weechat relays'\" ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDZKYLFI5eyv4/aZR8YUBLQagMoH4gyltcXiom/ZA9XpgtKy3fOxmX0NzETxVBdbc/mhU6s9zKNBXWDl5OS17a+GNPStPpUjMGB/ehTK+FkcNOHsqZ60QLHzejWbMfOFiZ9idPsVpvWl8o95YzUAZc61NlVMR1r9fys1CZtFk6ZfYuGLofnG3BafCeMY6BkWZ7Hi/+AoWxe57CIJzwmy3aBpQ7NxpS5vZl8/DGThLuRK4Ew6w1/TmRTTJjTr0USKPSI1V3XYZDkN3BZ4dRyIerpMEPhYfsLum1Qj+Oc60EziERsSadk4UnJt3ye4VjTcv+1za/CeDz6zAjgpEbfEBAN8xpNsfdjeFedw32YGRFhWpC2xg/yho7c2n8w5IOWKObggKZYxLBatzKpg0eYO5B/jBNnM6HIW/wfiKunnYrJMJHwzFSYZCyIRlZKb6yWRcW1+RlvlmlE95bAxPWmrFbEtOoPVESAR6lfTleKPMgjTGJ3KvYzp1k8jplJcQpbPmMiejkheh3tcKWn/Na3EfV3pUWGHtU6CQauHYdDBAfHnAD9baAHEpy3HGYz+0cgH4kKSas4U8ICaFeOE3OEv90caHyGHWaL5ynvq4ywUAlJUWYLIseHq4+aSxKj8aK/Nr6zfDUNAAOEaveT/YUeFk5LxaEXowkW/98nUwZ//G7vKw== talyz@android-2017-03-03"

      # Computers
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPqiNApj9AcGJ4JfsDTdhSlfwmuDzKJmVrOGJibYKOo5 root@raine-2017-02-18"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIC0Ny2gtIq74qiHph/5ZUyNkdVTTXj2lnuDRHpgR3fPi talyz@flora-2017-01-10"
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQClEBWe28hb+GRtpa0MdKh/2UKPIANBEeDuXlcfHgS4Mi4gTNKMWjclXhBzEYfRLpYEZuBjcciRTKNKUjMi4aSmc3g6/FoueaDTmHhoQWCEFwrR7m1ZwplHYZuad2Dm6kOMXyi3VIw1y4u3K832LsTrrgBeDT+C23qVfjvmeytD7tWnoEqgDKnrdxZYqiNdu43HA5V7r7jXCMVby2/39iqa+AxKBxt/v1gz7rar3jr/6EfE55oJpQfj8wFGLq88IK915eTTEVYSZLUxZkfOaZGMjkyMiXNTLWtJ/MfBQ0SagDwwuZKf/+C/O1vO6scz6Uc9wBUPPbUnhUkGzO/kWduXiXLQfEwYrFVAd2HyrErwnuJs/0HSWYm/c6o4O1xaDaqj+bcfGewK+EEPU+J1P0MwXTLgpJ6r0VkzrKd/r0kVUrxXqhrMdSwtl6M3CgqDc3rFgiV5xs4nRjnwbhchud77ktZ3zV40uLYXHa5IlldN4O91MD1+LVffc5eceJmhn9ivuoEk+w/Wwtk8c/G2axakfmF9H4VFRgzyVnKrel2Gz4gZd1wihA2B8o4eh10pEmeS5O0BRDXpJGMC3FKCelX42mEYy4qr6bCF4Bqo0+bQOHgzZpdQQ+utmvrYlMVVcJMqh2xjSbaPdC+trOa0fvVBFTXIAF/Wn/1zFj6+G6mCYQ== talyz@zen-2018-11-16"
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQCxpS0/cz+daRaLFagSCK9SEd5bq8wmZs+yUbktgsiMQsfh3fT8kK5P7O7DjZBJMrPRwXJU6BNCGpe08755kdVw1gNfDDUyiUznqM1Q3Uzvb0dvOMm16RgLRb8da8ilNxIVXI5cO0MKzZJM06aTZfPyP9bFIUCIL9DP3wu91ts+vlBuOCcFymzf7B5uTZUpHaoq2Aq8+xeBUnF9stdBJ35yJihIIZCIZ2hSMpfV7lrKgyzgUJugO9WoGIsKgaro2r8Em7IiWdAfLa/OAunEw7Crau4sJrwBXLSqRxor/H8wUHWzfDqm1YvL1yMShVe4Fv/2V7uXxhsA5xHbboGe7tkPaURaUhySgUxycjZhFT/fNqJyU0/xUyrJMGA+5Ml9dY3NeTMJKUBeyt18yj8gAh4gqRzLtcsgSveQB1h5nVYO3xP1ydhLYrfZ4XJkEhSix8YhfEr+pxCwSC/9cx2w/H+10aQgQJqI15llfBL1Rl24g0f+VcawuWdahRL0sDUYQLBt5FRNn/SKBu/PGGU5XW4Ox0Zw7wSJZ0ukPORwaRFPgMC2IpDk14bqBEG6VVWYuSSq01IOAnwNqcrcsKFXA/+9OV5wgYOpxjQwIx5bA41T8RsbrrgENMAlm3VkUOgWz50wuMQfWR7fXrmqehsvmmG/fJo9ro56W908y9val2cCVw== talyz@flora-2019-06-24"
    ];

    # Public keys of different hosts
    systems = {
      # aarch64.nixos.community
      "aarch64.nixos.community" = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMUTz5i9u5H2FHNAmZJyoJfIGyUm/HfGhfwnc142L3ds";

      # Private laptop
      laptop-private-elis = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOr9fpRag0ZQq3eMOPHygrt60GZl0NW32rzvvvgsm5HC";

      # home.elis.nu
      server-main-elis = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJRZYWjxAqloB5MZtxBHkckZhKi+3M1OObzBdyi7La98";
      server-main-elis-initrd = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGii+3fHNc3to81E0kY+W1yvPCnjFoMZxUr+SbH2nx1e root@fenchurch";

      # vps04.elis.nu
      vps04 = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIC/UewM3gYrEFZdaD6zDdP9Vkq1W9gOIXJoBG/ram+Fo";

      # vps06.elis.nu
      vps06 = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINB1+Am7Ai9DfKjDv7JDmPA711FW9wrOXRGZZf0rmjTP";

      # Work laptop
      laptop-work-elis = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMQFSZBEijplTEV9Vag79O0rrYhkpmy6++w2yb2RG4qP";
    };

    # Guest user
    guests = [
      # Adisbladis
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCtr+rcxCZBAAqt8ocvhEEdBWfnRBCljjQPtC6Np24Y3H/HMe3rugsu3OhPscRV1k5hT+UlA2bpN8clMFAfK085orYY7DMUrgKQzFB7GDnOvuS1CqE1PRw7/OHLcWxDwf3YLpa8+ZIwMHFxR2gxsldCLGZV/VukNwhEvWs50SbXwVrjNkwA9LHy3Or0i6sAzU711V3B2heB83BnbT8lr3CKytF3uyoTEJvDE7XMmRdbvZK+c48bj6wDaqSmBEDrdNncsqnReDjScdNzXgP1849kMfIUwzXdhEF8QRVfU8n2A2kB0WRXiGgiL4ba5M+N9v1zLdzSHcmB0veWGgRyX8tN"

      # Kira
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDGYkUq4mlkK2fd2SOS1CzLZUAgyIjLZOiAn0fJav8F1GkorjInzCs9AIUGYRA2xAPWNVrbpXRYTXkrQqmiDwS8Gda6gS6tMd+XvUDs2i2tk3rjvxe6V3nQRZBxqXDOPLE7bznQVIDwItHp51+SetJW0iZbFWkWGQEKlB1RzEyNo8rtaTgi6S+6Ymz4BYOiXd/7ChCSl6n64dsX7EYyONoh45MjDqvYSq6P6dpZWHafNbPkEAJ+5pBiW46KqvSDg3otc7z17VrSEtdw8R+GL1nK0Jo5Y1ZfPZRXAxdR5wbS/fAwPIuEiweGrWbzHmUuTD5LlFi/clVpsrsTVToiMEf42gjC3gWj0eTQ2xRr1eVbiUaCECAgX8D1G1jc9Bsl+6CLMQUg+7Jeu4IaxC1Kpque/KiYv7JoCYXSR0s16lITFUdcvcERxhdEuf1NQl7Zm206rN+AtaIG7pRkdwWAc4QjK1eurli4oYcsAGT8qd1hnQZ+9tbik5TFRgT5Ey6255ILBn9Io7AFFno6ekt0s2CXFMNp8DcogNWNePHM+vNucmRmh+t4GN84JiRL2DH3cTQwEiVBpHkSFESq1jSTjkr1VERKNjSMJSU11K0l+PQhecANisFXxkSfeEcDCCLPxCYWbFJGjWw4MdAr63mgkZYf6ro9GYlGjzTk93DWIOw13w== erik.welander@hotmail.com"

      # David
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACEwMbcK/A2oyO1yD746jP8joXPlnXHfg7HFcrNd3pmhI5MVSjzlG3v6Xlqyw06jKSzrONPM3OoItwm422P/hNgj/4TShUrTT2JeilpTL7E+z7r28Kgn/ijqTBLrQY3INvGV4saYIBvpmUD+JcN1sDivsNME4jD5Skw/o8PX6s2AvxyihwdAlur41mtKNAiB7pf8XosmhBnYZciBoXQ0dRdkoH1ZP1D/Y8r0eaaP8tUh7K+fHk5zX91bLlahnmjVdHJK42KMkJ9+sJ6P4phKvrefSxlgSHYMp/kib4a8gVMg3F7T1tdx+0aqlG06ruB7U/QP/fp25QOahSuBaFvP3oHcuZAXiRnOi6/uIdWmBA3FHEJPjfjZe10uRJDHWfyC5D05yZlm9q0Ejo1RL47XLYUg8KQvzKAXFWnHYVdGWdaQYQMRrMzqtJyyCa22uuPzqMkU+KamDrgv0v1T59/dNLfjIAiEW9WeVg5q795nSKDDDRkIJJ/5dfKnAQZMi8clKfnpVocK7crKsi1d2YtM/9zvsR6CDIoKVX1UNCMI7UMsRuI3a4FfjNpbn5EDsfln3E//DS8p5U1MA7vn6LRTjRbtHxakWWinQ+uekB6ERGpg9xb3O90HYsE9BkVZdasF8FKY2cWaB7JtIkYY/bkNO+CiVpiFG4jJYH064IadZQJhEMniJkKfsOtomBnV9s3E5OosSpfw== David"

      # Tjodden
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICq8ZWhjJXWkfnF8sYZcEDDxzkg7khqof1skvGn8ntZy tjodden@dellen"
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQClKmXlecsQMO3CoC6oCslxfHN4i2hQVgD7BKMRWQSpOvRi4K+n09MDe/8Bv4MkKDTYleG7unsBMdcrrSgThNUSWoft3bidSDMyrnkD9uI+q/pi9sIIQD+HptrrR7ApfCxQh/RCkNaLkVYhNZA+m+3XKTgAVEGP7MxpzknkTpmlzHPKpeSbRW4o8w/ffgp/h+mkDH9wpzbmjVDHFGShtrYsOGfX8N4b+ojCLFO0kAtJojvyCD6zdVjrWlcOh76/cWzOOWeD750vh85kzCbTtlXopL4B11QjBiM8Xy58aaIIKCecJ8pX63UDtMze44uMrS4BanXu+gsb0bLT45mgL/FTMPhdSxx0th6VxQfvWmOLeo8DvH1KkF7+FrJ4PFenMav9NDYMfRMUkL2OB8PnTCqyPyOpzMH3KRPaKxxAMDiUPxNFq0Et4+KwTBPMvErDrOb57rAeR/UNNbGlwc/we6Lk8amj5tusQgAtEjo2H61gSC0IAEXUtJOaQB8ZjzU3Kuu6fgreJkFhUrRh5PTGlvlCT0bWyA5tWIh8HwF4XI1SgAp3Fgyoq5AyKIXaVIjNw4cfiizAbPVkX1hWISGp1xv1TaLu3A0D4Ph3arbdv74iSrp7j2mg/rQOklnFsJkYxdbTNazlAopR+Qs9BC2azXW4idkQ9U6rZ8JR5zSvNgDdFw== tjodden@dellen"

      # CoopDot
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQC/Y3PXxqlSa9TFH0d7LVcBzOVO11zidbt9wgzxm88gFWxD+vk+ctQZZ2HjQXD+KQkyLHPMukALDiXESW8P3ecDxqy70UjRlFUKzLouFvg2bM2ghMVfNvhgCUYJ1kN6EJGAYX4VgR2ivONRiW3I65sV1/nC4u7ZJjA2sERPP4p8OemCsJRybXs9SmkF4xjLRFjK51UnnuRyXwZWVcYipUvsbBq3Y6ULHvuqk64h8MQs0N4mzr2qeZ+Kko8/zq0KjznHJ86WLLCXXMxZKP5Eo99WyTqCbAyGzoC2ojnls2SKAN7e3hLJfjdqq+HKhFhED4odmk6xfb4VZQpv3vqXAZh9 coopdot@darke"
    ];
  in {
    inherit etu concate talyz systems;

    guests = etu.computers ++ concate ++ talyz ++ guests;
  };
}
