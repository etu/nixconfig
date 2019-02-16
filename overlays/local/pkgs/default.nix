self: super:

with super.lib; {
  flummbot = super.callPackage ./flummbot { };
  ip-failar-nu = super.callPackage ./ip-failar-nu { };
}
