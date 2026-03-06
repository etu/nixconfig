let
  etu =
    let
      # New fileserver
      server-main-elis = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDvsU9DbT9Buk0FcEA6cuq8UdE1wG+bD0UpyGoxJc93x etu@fenchurch-2019-12-22"
      ];

      # Desktop computer
      desktop-elis = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKm0NHTsWdN+R+Ksvvva6FTZ9kVPexQpIGm7+6HGmX7q etu@desktop-elis-2024-11-15"
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
        server-main-elis = [
          "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMGc+oDfq+OCsApi1qsMDx1wlDwfu7oIHOeV0laVdq6W syncoid@fenchurch-2020-07-11"
        ];
        workstations = [
          "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICdtbbrBHT1i29nFdaf54zsJ4Yrt2tOLnNotaRIneazp syncoid@workstations-2021-07-11"
        ];
      };

      # Github actions deployment key.
      github-actions = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBsVq+lSP7EuU0KUurWYjlLWm1PJWKtYUXVayi1jD6lU github-actions-deployment-2023-08-30"
      ];
    in
    {
      # Include all separate units
      inherit
        desktop-elis
        server-main-elis
        syncoid
        laptop-private-elis
        laptop-work-elis
        github-actions
        ;

      # Include a meta name of all computers
      computers = desktop-elis ++ server-main-elis ++ laptop-private-elis ++ laptop-work-elis;
    };

  concate = [
    # Computers
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDJR9Mm7KTTxZ1A9xIqv8RyGB5Yp08RE3Ns0M4fsyTLjdIw08RjD+Up7PA1KKgoGsTTzk9670lrQVohpUE96MeoteZnZjFUEGv1Oiuy4yVs/jy5ngwtdchh6MU+xOvnhZ/7m9inoRFJXG1OTEsipKVrCXctjNNPY7XIM99QH7wH2Ebua8H5QFnvclTIpN2d/QhhHLlffye++ww5lltU4AkSt72QlAqUOefsVdQDS3TuXxu1+OgQFnK8eaz517pktsLkDEY/9lkqSNP2okTTmaj928TEZHYDYt+IkoMZxoGO8iCFtKsJY24gSdl3qysVPVWwPpS1TxV/sud5iPQzBXfHRtbNEnQ6ewImLsvkFmGYhIQ3gJmwHbRSUtg7TJ5TZOEez8Wc6cFOwqE7ltlM8ob4etPcmo3qiJH6ecchOwwrY4HWPdw+uYBAvFVaNEEf/elWjoZTC6BSb7Bf8sba8m/98+QYj0LWL0FDDarlx6ZUnkbROk44WlmI6nk+MVKvb/rjK/sbATxruj9ARWhQUXgA6PFytmEj5OhSK5ibcRDB23cTMk+DK5jldDLLPP7gi6NlhhdfoBJEtgXNJqCDrpgf8yLG7bXJYWroMArRFJXcQc4Ogxd10K5Ec6TDPvwDqrMoFEMW1X20jZkdF+4wqcQUs2CD96iHao+QxxYoScgV7w== caroline@neji-200521 "
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBSb9gF46pEMA0hZzR6/EFbXzsV10RtB2dcm3HFCz0Ob concate@rocklee"
  ];

  # Public keys of different hosts
  systems = {
    # aarch64.nixos.community
    "aarch64.nixos.community" =
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMUTz5i9u5H2FHNAmZJyoJfIGyUm/HfGhfwnc142L3ds";

    # Desktop computer for Caroline
    desktop-caroline = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPGp2BbNa2Jum5qaIfuxsBgiqBi6wOiclGBziq9B/2lD";

    # Desktop system
    desktop-elis = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIT37fYpCIdvXtvAVhsbx3fVMp+2ve50BXy5svsC4du3";

    # Private laptop
    laptop-private-elis = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOr9fpRag0ZQq3eMOPHygrt60GZl0NW32rzvvvgsm5HC";
    laptop-private-caroline = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGCHcHcxKgufBHM2vrpScbyUCLKbo8qh9RUc7rqdf92u";

    # home.elis.nu
    server-main-elis = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJRZYWjxAqloB5MZtxBHkckZhKi+3M1OObzBdyi7La98";
    server-main-elis-initrd = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGii+3fHNc3to81E0kY+W1yvPCnjFoMZxUr+SbH2nx1e";

    # Sparv server
    server-sparv = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICHgZaEnCWXVULHjWqgsvf3mQDX20WmWzAagAtHsBEMZ";

    # vps06.elis.nu
    vps06 = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINB1+Am7Ai9DfKjDv7JDmPA711FW9wrOXRGZZf0rmjTP";

    # Work laptop
    laptop-work-elis = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMQFSZBEijplTEV9Vag79O0rrYhkpmy6++w2yb2RG4qP";
  };
in
{
  inherit etu concate systems;
}
