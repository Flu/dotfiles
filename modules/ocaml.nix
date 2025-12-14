{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    # Basic Ocaml tools and compiler
    ocaml
    opam
    dune_3

    # Opam packages
    ocamlPackages.ocaml-lsp
    ocamlPackages.ocamlformat
    ocamlPackages.zarith
    ocamlPackages.core
    ocamlPackages.lwt
    ocamlPackages.cmdliner
    ocamlPackages.yojson
    ocamlPackages.sexplib
  ];
}
