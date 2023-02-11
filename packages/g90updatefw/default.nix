{
  fetchFromGitHub,
  buildGoModule,
  ...
}:
buildGoModule {
  pname = "g90updatefw";
  version = "2021-11-22";

  src = fetchFromGitHub {
    owner = "DaleFarnsworth";
    repo = "g90updatefw";
    rev = "fd69662a7ced939ab231c5d255984bb5393f2a68";
    sha256 = "sha256-8hOWx2WNL8N4j/20eVtvQ5+oHajzYufxvXcRziwxsj8=";
  };

  vendorSha256 = null;
}
