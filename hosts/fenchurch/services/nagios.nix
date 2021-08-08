{ config, pkgs, ... }:

let
  contactsCfg = pkgs.writeText "contacts.cfg" ''
    define contact {
      contact_name admin
      use generic-contact
      alias Elis Hirwing
      email elis@hirwing.se
    }
  '';

  hostgroupsCfg = pkgs.writeText "hostgroups.cfg" ''
    define hostgroup {
      hostgroup_name public
      alias          publicly accessible servers
      members        *
    }
  '';

  hostsCfg = pkgs.writeText "hosts.cfg" ''
    define host {
      use       linux-server
      host_name home.elis.nu
      address   127.0.0.1
      contacts  admin
    }

    define host {
      use       linux-server
      host_name vps04.elis.nu
      address   116.203.135.146
      contacts  admin
    }

    define host {
      use       linux-server
      host_name vps05.elis.nu
      address   116.203.56.235
      contacts  admin
    }
  '';

  servicesCfg = pkgs.writeText "services.cfg" ''
    define service {
      hostgroup           public
      service_description Check host is alive
      check_command       check-host-alive
      use                 generic-service
      contacts            admin
    }

    define service {
      hostgroup           public
      service_description SSH
      check_command       check_ssh
      use                 generic-service
      contacts            admin
    }
  '';
in
{
  services.nagios.enable = true;
  services.nagios.objectDefs = [
    "${pkgs.nagios}/etc/objects/commands.cfg"
    "${pkgs.nagios}/etc/objects/contacts.cfg"
    "${pkgs.nagios}/etc/objects/templates.cfg"
    "${pkgs.nagios}/etc/objects/timeperiods.cfg"
    contactsCfg
    hostgroupsCfg
    hostsCfg
    servicesCfg
  ];

  # Make sure to have nginx enabled
  services.nginx.enable = true;

  # Include agenix encripted secret
  age.secrets.nagios-elis-nu = {
    file = ../../../secrets/nagios-elis-nu.age;
    owner = "nginx";
  };

  services.nginx.virtualHosts."nagios.elis.nu" = {
    forceSSL = true;
    enableACME = true;
    root = "${pkgs.nagios}/share/";
    extraConfig = ''
      index index.php;
      auth_basic "Nagios";
      auth_basic_user_file /run/secrets/nagios-elis-nu;
    '';
    locations."/".tryFiles = "$uri /index.php";
    locations."/nagios/".alias = "${pkgs.nagios}/share/";
    locations."~ \.cgi$" = {
      root = "${pkgs.nagios}/sbin/";
      extraConfig = ''
        rewrite ^/nagios/cgi-bin/(.*)$ /$1;

        include ${pkgs.nginx}/conf/fastcgi_params;
        include ${pkgs.nginx}/conf/fastcgi.conf;

        fastcgi_param AUTH_USER       $remote_user;
        fastcgi_param REMOTE_USER     $remote_user;
        fastcgi_param SCRIPT_FILENAME ${pkgs.nagios}/share/sbin$fastcgi_script_name;

        fastcgi_pass unix:${config.services.fcgiwrap.socketAddress};
      '';
    };
    locations."~ \.php$" = {
      tryFiles = "$uri =404";
      extraConfig = ''
        include ${pkgs.nginx}/conf/fastcgi_params;
        include ${pkgs.nginx}/conf/fastcgi.conf;
        fastcgi_split_path_info ^(.+\.php)(/.+)$;
        fastcgi_pass unix:${config.services.phpfpm.pools.nagios.socket};
        fastcgi_index index.php;
      '';
    };
  };

  # Fcgi wrap
  services.fcgiwrap.enable = true;
  services.fcgiwrap.user = "nagios";
  services.fcgiwrap.socketAddress = "/var/run/fcgiwrap.sock";

  # Set up PHP Pool
  services.phpfpm.pools.nagios = {
    user = "nagios";
    settings = {
      "listen.owner" = config.services.nginx.user;
      "pm" = "ondemand";
      "pm.max_children" = 2;
      "pm.process_idle_timeout" = "60s";
      "pm.max_requests" = 200;
    };
  };
}
