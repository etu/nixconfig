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
    in
    {
      # Include all separate units
      inherit
        desktop-elis
        server-main-elis
        laptop-private-elis
        laptop-work-elis
        ;

      # Include a meta name of all computers
      computers = desktop-elis ++ server-main-elis ++ laptop-private-elis ++ laptop-work-elis;
    };

  # Public keys used for syncoid.
  syncoid = {
    server-main-elis = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMGc+oDfq+OCsApi1qsMDx1wlDwfu7oIHOeV0laVdq6W syncoid@fenchurch-2020-07-11"
    ];
    workstations = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICdtbbrBHT1i29nFdaf54zsJ4Yrt2tOLnNotaRIneazp syncoid@workstations-2021-07-11"
    ];
  };

  # Github Actions deployment key.
  github-actions = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBsVq+lSP7EuU0KUurWYjlLWm1PJWKtYUXVayi1jD6lU github-actions-deployment-2023-08-30"
  ];

  # Public keys of different hosts
  systems = {
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
  inherit
    etu
    github-actions
    syncoid
    systems
    ;
}
