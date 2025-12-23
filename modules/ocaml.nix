{ pkgs, ... }:

let
  ocamlPkgs = pkgs.ocamlPackages;
  ocamlLibDir = "${ocamlPkgs.ocaml}/lib/ocaml";
in
{
  environment.systemPackages = with pkgs; [
    # Basic Ocaml tools and compiler
    ocaml
    opam
    dune_3
  
    # Opam packages
    ocamlPkgs.ocaml-lsp
    ocamlPkgs.ocamlformat
    ocamlPkgs.findlib
  ];
}
