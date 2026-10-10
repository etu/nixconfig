let
  etu =
    let
      # New private laptop T495
      laptop-private-elis = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKALrQoSasNAaAvERCMsztZkezg0gRSFXWbc1vXpA1+C etu@laptop-private-elis-2023-01-27"
      ];

      # User key on the new work laptop
      laptop-work-elis = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINPQrlHiejXvToMVQxO/HOhqYjl5yqC8rTLeT7maCLeb elis-work-laptop-2026-10-08"
      ];
    in
    {
      # Include all separate units
      inherit
        laptop-private-elis
        laptop-work-elis
        ;

      # Include a meta name of all computers
      computers = laptop-private-elis ++ laptop-work-elis;
    };

  # Public keys used for syncoid.
  syncoid = {
    server-main-elis = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMGc+oDfq+OCsApi1qsMDx1wlDwfu7oIHOeV0laVdq6W syncoid@fenchurch-2020-07-11"
    ];
  };

  # Github Actions deployment key.
  github-actions = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBsVq+lSP7EuU0KUurWYjlLWm1PJWKtYUXVayi1jD6lU github-actions-deployment-2023-08-30"
  ];

  # Mittens
  mittens = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIF0+Vj2l+oYL4BtKG/92rySkcjx2WHgGBn8L5nfYv1mD"
  ];

  # Public keys of different hosts
  systems = {
    # Private laptop
    laptop-private-elis = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOr9fpRag0ZQq3eMOPHygrt60GZl0NW32rzvvvgsm5HC";

    # home.elis.nu
    server-main-elis = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJRZYWjxAqloB5MZtxBHkckZhKi+3M1OObzBdyi7La98";
    server-main-elis-initrd = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGii+3fHNc3to81E0kY+W1yvPCnjFoMZxUr+SbH2nx1e";

    # Sparv server
    server-sparv = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICHgZaEnCWXVULHjWqgsvf3mQDX20WmWzAagAtHsBEMZ";

    # vps06.elis.nu
    vps06 = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINB1+Am7Ai9DfKjDv7JDmPA711FW9wrOXRGZZf0rmjTP";
  };
in
{
  inherit
    etu
    github-actions
    mittens
    syncoid
    systems
    ;
}
