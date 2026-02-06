_: {
  home.file = {
    # Mpv config file - Don't show images embedded in music files
    ".config/mpv/mpv.conf".text = "no-audio-display";

    ".XCompose".text = ''
      include "%L"

      # Default already
      # <Multi_key> <a> <a>: "å"
      # <Multi_key> <A> <A>: "Å"

      # Some nice binds
      <Multi_key> <a> <e>: "ä"
      <Multi_key> <A> <E>: "Ä"
      <Multi_key> <o> <e>: "ö"
      <Multi_key> <O> <E>: "Ö"

      # Table flip multi key
      <Multi_key> <t> <f>: "(ノಠ益ಠ)ノ彡┻━┻"

      # Shruggie
      <Multi_key> <s> <h>: "¯\\_(ツ)_/¯"
    '';
  };
}
