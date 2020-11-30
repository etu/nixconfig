{ pkgs ? (import <nixpkgs> { }) }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    niv
    nixpkgs-fmt
  ];
}
