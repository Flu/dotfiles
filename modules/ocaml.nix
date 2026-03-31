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
    ocamlPkgs.integers
    ocamlPkgs.lwt
  ];

  environment.variables = {
    OCAMLPATH = "/run/current-system/sw/lib/ocaml/${pkgs.ocaml.version}/site-lib";
    CAML_LD_LIBRARY_PATH = "/run/current-system/sw/lib/ocaml/${pkgs.ocaml.version}/site-lib/stublibs";
  };
}
