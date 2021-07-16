{ config, lib, pkgs, ... }:

let
  cfg = config.programs.weechat;

  # Function to flatten attributes.
  #
  # Shamlessly stolen from:
  # https://git.qyliss.net/nixlib/tree/modules/workstation/weechat/default.nix
  flattenAttrs' = sep: attrs: (
    lib.listToAttrs (lib.concatLists (lib.flip lib.mapAttrsToList attrs (k: v:
      if lib.isAttrs v then
        lib.mapAttrsToList (k': lib.nameValuePair "${k}${sep}${k'}") (flattenAttrs' sep v)
      else [ (lib.nameValuePair k v) ])))
  );

  # Set up the function to flatten with a "." as separator.
  #
  # Shamlessly stolen from:
  # https://git.qyliss.net/nixlib/tree/modules/workstation/weechat/default.nix
  flattenAttrs = flattenAttrs' ".";

  # Format values to weechat compatible values.
  #
  # Also shamlessly stolen from:
  # https://git.qyliss.net/nixlib/tree/modules/workstation/weechat/default.nix
  toWeechatValue = value: (
    if      value == true    then "on"
    else if value == false   then "off"
    else if lib.isList value then lib.concatStringsSep "," value
    else                          toString value
  );

  # Build weechat commands to run in weechat-headless (as a list)
  commands = (
    # Add servers before adding other config options.
    (map (
      network: "/server add ${network} ${toWeechatValue cfg.configs.irc.server."${network}".addresses}"
    ) (lib.attrNames cfg.configs.irc.server)) ++
    # Set config options to values or unset them if it's something
    # you want to remove, say default aliases.
    (lib.mapAttrsToList (name: value: (
      if value != null then (
        "/set ${name} ${toWeechatValue value}"
      ) else (
        "/unset ${name}"
      )
    )) (flattenAttrs cfg.configs)) ++
    # Add ignores configured
    (map (args: "/ignore add ${args}") cfg.ignores) ++
    [ "/save" "/exit" ]
  );

  # Run weechat headless and set all the options, then save the options
  # and exit weechat headless so we can use those generated config files.
  #
  # But override the init instead of using --run-command so scripts
  # loads before the commands runs. Otherwise you can't set settings
  # for scripts.
  configs = pkgs.runCommandNoCC "weechat-config" { } ''
    ${cfg.package.override {
      configure = { ... }: {
        inherit (cfg) scripts;
        # Concatinate the weechat commands to a single command.
        init = lib.concatStringsSep ";" commands;
      };
    }}/bin/weechat-headless --dir $out

    # Remove all non-configuration files
    find $out -not -name "*.conf" -mindepth 1 -maxdepth 1 -exec rm -r {} \;
  '';
in {
  options.programs.weechat = {
    enable = lib.mkEnableOption "weechat";
    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.weechat;
      description = "Weechat package to use.";
    };
    ignores = lib.mkOption {
      type = lib.types.listOf lib.types.string;
      default = [];
      example = ''
        [ "<nick> [<server> [<channel>]]" ]
      '';
      description = "A list of strings to send to /ignore add arg";
    };
    scripts = lib.mkOption {
      type = lib.types.listOf lib.types.package;
      default = [];
      example = ''
        with pkgs.weechatScripts; [ colorize_nicks ]
      '';
      description = "List of script packages to install for weechat";
    };
    configs = lib.mkOption {
      type = lib.types.attrsOf lib.types.anything;
      default = {};
      example = ''
        {
          # Remove two default aliases
          alias.cmd.aaway = null;
          alias.cmd.anick = null;

          # Set some flags
          weechat.look.mouse = false;
          weechat.look.nick_completer = ": ";
          weechat.bar.buffers.color_bg = "default";
          weechat.bar.buffers.color_bg_inactive = "default";

          # Set default nick names
          irc.server_default.nicks = [ "user" "user2" "user3" "user4" ];

          # Add a network
          irc.server.oftc.addresses = "irc.oftc.net/6697";
          irc.server.oftc.ssl = true;
          irc.server.oftc.autojoin = [ "#test" "#test2" ];
        }
      '';
      description = ''
        Here you can define weechat style configuration option names,
        with some added sugar, such as nix lists and real booleans.

        The options are the same as the ones that exists inside of
        weechat. They can can be discovered within weechat config
        documentation, config files and within weechat itself.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    warnings = if (lib.hasAttrByPath [ "sec" ]  cfg.configs)
      then [ ''
        Do not try to set things in sec.conf using these configs,
        instead you should create that file in a separate manner and
        provide that file in the correct location
        ~/.config/weechat/sec.conf in your own way.

        Then you can use secrets in any other option using weechat's
        secret variable syntax that would look something like
        this: ''${sec.data.my-option}
      '' ]
      else [];


    home.packages = [
      # Install weechat with scripts
      (cfg.package.override {
        configure = { ... }: {
          inherit (cfg) scripts;
        };
      })
    ];

    # Add configured files only to the xdg config file path.
    xdg.configFile = builtins.listToAttrs (lib.mapAttrsToList (
      name: value: {
        name = "weechat/${name}.conf";
        value.source = "${configs}/${name}.conf";
      }
    ) cfg.configs);
  };
}
