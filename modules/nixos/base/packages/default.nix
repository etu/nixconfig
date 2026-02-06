{ pkgs, perSystem, ... }:
{
  # Install some command line tools I commonly want available
  environment.systemPackages = [
    # Nice extra command line tools
    pkgs.bat # "bat - cat with wings", cat|less with language highlight
    pkgs.btop # CLI system monitor
    pkgs.curl # curl duh
    pkgs.duf # nice disk usage output
    pkgs.fd # find util
    pkgs.file # file duh
    pkgs.fzf # fuzzy finder
    pkgs.jc # parse different formats and command outputs to json
    pkgs.jq # parse, format and query json documents
    pkgs.ncdu # disk usage navigator
    pkgs.pv # pipe viewer for progressbars in pipes
    pkgs.ripgrep # quick file searcher
    pkgs.testssl # print TLS certificate info
    pkgs.speedtest-cli # Speedtest command line util

    # Own tools:
    pkgs.nur.repos.etu.llr # llr, tool to cut long lines
    pkgs.nur.repos.etu.mkvcleaner # mkvcleaner, clean video files from unwanted tracks

    # Networking tools
    pkgs.dnsutils # dig etc
    pkgs.host # look up host info
    pkgs.whois # whois duh
    pkgs.prettyping # pretty ping output
    (pkgs.runCommand "prettyping-pp" { } ''
      mkdir -p $out/bin
      ln -s ${pkgs.prettyping}/bin/prettyping $out/bin/pp
    '')

    # Install some color test scripts from xterm
    (pkgs.runCommand "xterm-color-scripts" { } ''
      tar -xf ${pkgs.xterm.src}

      install -Dm755 xterm-${pkgs.xterm.version}/vttests/256colors2.pl $out/bin/256colors2.pl
      install -Dm755 xterm-${pkgs.xterm.version}/vttests/88colors2.pl $out/bin/88colors2.pl
    '')

    # Package to do kexec to other systemd-boot nixos generations
    perSystem.self.nixosSystemdKexec
  ];
}
