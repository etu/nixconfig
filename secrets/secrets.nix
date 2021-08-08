let
  # Import my ssh public keys
  keys = import ../data/pubkeys.nix;

  # Assemble public keys for user-facing computers
  etu = keys.etu.agrajag ++ keys.etu.work;

  # Computers host keys
  hosts = let
    fenchurch = [
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDXPQN5sjBXNKPMT7SzGIywlS5zupc2p/6uYiUxSzDzN39hhZDt5iKVbKTvmtSvHU3b/bOLA1U3DKvVZPbSyuaddmN7Oo0dh4EaN8dkpSkH5xuWb7bwrrZ9ZI4Ggp0NgnYBcPp5xWs8jrJbhnFQDS/Ev71UAXeRRhcNu6QtHKJrcLPh+FTw4HkoWDD7E9n0kFovBzpkm9bm9tRiuuUzKu8d1ue+h3e3L7Wku6jhsM4xDKflv1I8O3HH6zgA+cdy3SJ1QDMBizTHzkW7tuWTjayspSrNAYnm3Yn4u5KBIPLtB1F6xe+bC8FAp59ZzflGHzwtz2psj4OlnWjmiqUde+cxsk4012neNf2UN2+q5w8mGN9BzETfxmwykCrKxc5nWk7yeaNx3A3SIJvT6hI+5tC+PZdY4FGFQm/StDeYXumFn0bjy+vW/1uLkF1AmsxbDmUO/Dj4cbx7Dh3m+5mCGQ6UyShD56lxKsdZBUTnawaK7WZhSiY72KS7EU9Li4Uk/hqJG/qcS+zmSo32ZpbwtIEOtRNm8ebTHjpvpv1vMY6n76R/xLJEWKwvE0K6EdGjD5RzrYWSIUVnsk+nf2/BNN6pGDesZhL6eltfKEyfJvBVwm3cPRLaE/kxehmcPFmLH4i1bijbDn4JpH7PWIQMyX+UF4+SpEhXtQ2UxdfUllZSow=="
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJRZYWjxAqloB5MZtxBHkckZhKi+3M1OObzBdyi7La98"
    ];
  in {
    inherit fenchurch;

    all = fenchurch;
  };
in
{
  "nagios-elis-nu.age".publicKeys = etu ++ hosts.fenchurch;
}
