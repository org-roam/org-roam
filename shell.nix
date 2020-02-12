let
  pkgs = import <nixpkgs> {};
in
pkgs.mkShell {
  name = "docs";
  buildInput = with pkgs; [
    mkdocs
    python3Packages.alabaster
  ];
}
